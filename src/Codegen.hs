{-# Language OverloadedStrings #-}
{-# Language LambdaCase #-}
{-# Language TupleSections #-}

module Codegen (generate) where

import Control.Monad.Writer
import Control.Monad.State
import Control.Monad.Except

import Data.Text (Text, pack, unpack)
import qualified Data.Text.Lazy.IO as TIO
import Data.Text.Lazy.Builder
import Data.List
import Data.Foldable (traverse_)

import Syntax
import Type

type Gen = ExceptT String (StateT GenState (Writer Builder))
data GenState = GenState
    { tmpVarCount :: Int
    , genBuffer :: Builder
    } deriving (Show)

generate :: FilePath -> TypedModule -> IO ()
generate file mod =
    let defaultState = GenState { tmpVarCount = 0, genBuffer = mempty } in
    case runWriter (runStateT (runExceptT (initGen mod)) defaultState) of
        ((Left err, _), _) -> print err
        (_, out) -> TIO.writeFile file (toLazyText out)

tmpVar :: Gen Builder
tmpVar = do
    state <- get
    let count = tmpVarCount state
    put (state { tmpVarCount = count + 1})
    return (fromText . pack $ names !! count)
    where
        names = map ('_' :) $ [1..] >>= flip replicateM ['a'..'z']

-- Helper state functions
out :: Builder -> Gen ()
out txt = do
    state <- get
    put (state { genBuffer = genBuffer state <> txt })

outln :: Builder -> Gen ()
outln = out . (<> "\n")

outBefore :: Builder -> Gen ()
outBefore txt = do
    state <- get
    put (state { genBuffer = txt <> genBuffer state  })

flushGen :: Gen ()
flushGen = do
    state <- get
    let buf = genBuffer state
    put (state { genBuffer = mempty })
    tell buf

collectBuffer :: Gen Builder
collectBuffer = do
    state <- get
    let buf = genBuffer state
    put (state { genBuffer = mempty })
    return buf

-- Code generation
initGen :: TypedModule -> Gen ()
initGen decls = do
    outln "// Juno compiler output"
    outln ""
    outln "// includes"
    outln "#include <stdlib.h>"
    outln "#include <stdio.h>"
    outln "#include <stdint.h>"
    outln "#include <stdbool.h>"
    outln "#include <math.h>"
    outln "#include <time.h>"
    outln ""
    outln "// typedefs"
    outln "typedef char unit;"
    outln ""
    outln "// program"
    flushGen

    genTopLevelDecls decls
    flushGen

    outln ""
    outln "// entry point"
    outln "int main() {"
    outln "\tjuno__main();"
    outln "\treturn 0;"
    outln "}"
    flushGen

genTopLevelDecls :: TypedModule -> Gen ()
genTopLevelDecls = foldr ((*>) . genTopLevel) (return ())

genTopLevel :: TypedTopLvl -> Gen ()
genTopLevel = \case
    TLFunc (TFunc ptypes rtype) name_ params _ body -> do
        let name = if name_ == "main" then "juno__main" else name_
        let (pnames, _) = unzip params
        let rtypeC = convertType rtype
        let paramsText = mconcat (intersperse ", " $ [ptypeC <> " " <> fromText pname | (pname, ptypeC) <- zip pnames (map convertType ptypes)])
        outln (rtypeC <> " " <> fromText name <> "(" <> paramsText <> ")" <> " {")
        flushGen
        out "return "
        genExpr body
        outln ";"
        outln "}"
        flushGen
    _ -> return ()

genDecl :: TypedDecl -> Gen ()
genDecl = \case
    DVar t _ name _ expr -> do
        out (convertType t <> " " <> fromText name <> " = ")
        genExpr expr
        outln ";"
        flushGen
    DStmt s -> genStmt s

genStmt :: TypedStmt -> Gen ()
genStmt = \case
    SRet expr -> do
        out "return "
        genExpr expr
        outln ";"
        flushGen
    SWhile cond body -> do
        out "while ("
        genExpr cond
        outln ") {"
        flushGen
        genExpr body
        outln ";"
        outln "}"
        flushGen
    SExpr expr -> do
        genExpr expr
        outln ";"
        flushGen

genExpr :: TypedExpr -> Gen ()
genExpr = \case
    ELit _ lit -> out (genLit lit)
    EVar _ name -> out (fromText name)
    EAssign _ l r -> do
        genExpr l
        out " = "
        genExpr r
    EBlock t decls res -> do
        cur <- collectBuffer
        flushGen
        traverse_ genDecl decls
        flushGen
        out cur
        genExpr res
    EIf _ cond texpr fexpr -> do
        out "("
        genExpr cond
        out ") ? ("
        genExpr texpr
        out ") : ("
        genExpr fexpr
        out ")"
    EMatch {} -> throwError "no match exprs yet"
    EBinOp _ oper a b -> do
        genExpr a
        out (fromText oper)
        genExpr b
    EUnaOp _ oper expr -> throwError "no unary opers yet"
    EClosure {} -> throwError "no closures yet"
    ECall _ fnexpr args -> do
        genExpr fnexpr
        out "("
        sequence_ (intersperse (out ", ") (map genExpr args))
        out ")"
    ECast _ targ expr -> do 
        out ("(" <> convertType targ <> ")")
        genExpr expr
    EDeref _ expr -> do
        out "*"
        genExpr expr
    ERef _ expr -> do
        out "&"
        genExpr expr
    ESizeof _ arg -> do
        out "sizeof("
        case arg of
            Left typ -> out (convertType typ)
            Right expr -> genExpr expr
        out ")"
    
genLit :: Lit -> Builder
genLit = \case
    LInt n -> (fromString . show) n
    LFloat n -> (fromString . show) n
    LString s -> (fromString . show . unpack) s
    LChar c -> "'" <> singleton c <> "'"
    LBool b -> if b then "true" else "false"
    LUnit -> "0"

-- Juno types to C types
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
    TPtr t -> convertType t <> singleton '*'
    TStr -> "char*"
    TChar -> "char"
    TBool -> "bool"
    TUnit -> "unit"
    TVar _ -> error "Parametric polymorphism not supported yet"
    other -> error (show other)

-- Utility
takeWhileOneMore :: (a -> Bool) -> [a] -> [a]
takeWhileOneMore p = foldr (\x ys -> if p x then x:ys else [x]) []
