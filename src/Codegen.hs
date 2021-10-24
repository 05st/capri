{-# Language OverloadedStrings #-}
{-# Language LambdaCase #-}
{-# Language TupleSections #-}

module Codegen where

import Control.Monad.Writer
import Control.Monad.State
import Control.Monad.Except

import qualified Data.Text as T
import qualified Data.Text.Lazy.IO as TIO
import Data.Text.Lazy.Builder

import Syntax
import Data.List

type Gen = ExceptT String (StateT GenState (Writer Builder))
data GenState = GenState
    { tmpVarCount :: Int
    , indent :: Int
    } deriving (Show)

generate :: FilePath -> [TypedDecl] -> IO ()
generate file decls = do
    let res = runWriter (runStateT (runExceptT (initGen decls)) (GenState {
        tmpVarCount = 0,
        indent = 0
    }))
    case res of
        ((Left err, _), _) -> print err
        (_, out) -> TIO.writeFile file (toLazyText out)

upIndent :: Gen ()
upIndent = do
    state <- get
    put (state {indent = indent state + 1})

downIndent :: Gen ()
downIndent = do
    state <- get
    put (state {indent = indent state - 1})

beforeStmt :: Gen String
beforeStmt = undefined

tmpVar :: Gen String
tmpVar = do
    state <- get
    let count = tmpVarCount state
    put (state {tmpVarCount = count + 1})
    return (names !! count)
    where
        names = map ('_' :) $ [1..] >>= flip replicateM ['a'..'z']

tellnl :: Builder -> Gen ()
tellnl = tell . (<> "\n")

initGen :: [TypedDecl] -> Gen ()
initGen decls = do
    tellnl "// Juno"
    tellnl "#include <stdlib.h>"
    tellnl "#include <stdio.h>"
    tellnl "#include <stdint.h>"
    tellnl "#include <stdbool.h>"
    tellnl "#include <math.h>"
    tellnl "typedef char UNIT;"
    genTopLevelDecls decls

genTopLevelDecls :: [TypedDecl] -> Gen ()
genTopLevelDecls = foldr ((>>) . genDecl) (return ())

genDecl :: TypedDecl -> Gen ()
genDecl = \case
    DFunc t name ps _ expr -> do
        let (TFunc pts rt) = t
        let pnames = map fst ps
        let params = foldr (<>) "" $ intersperse ", " [convertType pt <> " " <> fromText pname | (pt, pname) <- zip pts pnames]
        tellnl $ convertType rt <> " " <> fromText name <> "(" <> params <> ") {"
        tell "return "
        genExpr expr
        tellnl ";\n}"
    DVar t _ name _ expr -> do
        tell $ convertType t <> " " <> fromText name <> "="
        genExpr expr
        tellnl ";"
    DStmt s -> genStmt s
    other -> undefined

genStmt :: TypedStmt -> Gen ()
genStmt = \case
    SRet e -> tell "return " >> genExpr e >> tellnl ";"
    SExpr e -> genExpr e >> tellnl ";"
    SWhile c e -> do
        tell "while ("
        genExpr c
        tell ") "
        genExpr e
        tellnl ";"

genExpr :: TypedExpr -> Gen ()
genExpr = \case
    ELit _ l -> genLit l
    EVar _ v -> tell (fromText v)
    EAssign _ l r -> do
        genExpr l
        tell " = "
        genExpr r
    EBlock _ decls res -> do -- TODO
        tellnl "({"
        mapM_ genDecl decls
        genExpr res
        tell ";\n})"
    EIf t c a b -> do
        tell "(("
        genExpr c
        tell ") ? ("
        genExpr a
        tell ") : ("
        genExpr b
        tell "))"
    EBinOp _ op l r -> do
        case op of
            _ | op `elem` ["+", "-", "*", "/", "==", "!=", ">", "<", ">=", "<=", "||", "&&"] -> do
                genExpr l
                tell (fromText op)
                genExpr r
            _ -> undefined
    ECall _ f args -> do
        genExpr f
        tell "("
        sequence_ (intersperse (tell ", ") (map genExpr args))
        tell ")"
    ECast _ t e -> do
        tell $ "(" <> convertType t <> ")"
        genExpr e
    EDeref _ e -> do
        tell "*("
        genExpr e
        tell ")"
    ERef _ e -> do
        tell "&"
        genExpr e
    ESizeof _ t -> tell $ "sizeof(" <> convertType t <> ")"
    other -> undefined

genLit :: Lit -> Gen ()
genLit = \case
    LInt n -> tell (fromString $ show n)
    LFloat n -> tell (fromString $ show n)
    LString s -> tell $ fromString (show (T.unpack s))
    LChar c -> tell $ "'" <> singleton c <> "'"
    LBool b -> tell $ if b then "true" else "false"
    LUnit -> tell "0"

convertType :: Type -> Builder
convertType = \case
    TInt8 -> "int8_t"
    TInt16 -> "int16_t"
    TInt32 -> "int32_t"
    TInt64 -> "int64_t"
    TUInt8 -> "uint8_t"
    TUInt16 -> "uint16_t"
    TUInt32 -> "uint32_t"
    TUInt64 -> "uint64_t"
    TFloat32 -> "float"
    TFloat64 -> "double"
    TPtr t -> convertType t <> "*"
    TStr -> "char*"
    TChar -> "char"
    TBool -> "bool"
    TUnit -> "UNIT"
    TVar _ -> error "Parametric polymorphism not supported yet"
    other -> error (show other)
