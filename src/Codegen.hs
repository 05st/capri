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
import Data.Maybe
import qualified Data.Map as M

import Syntax
import Type

type Gen = ExceptT String (StateT GenState (Writer Builder))
data GenState = GenState
    { tmpVarCount :: Int
    , typedefs :: Builder
    , forwardDecls :: Builder
    , program :: Builder
    , genBuffer :: Builder
    , operMap :: M.Map Text Int
    , operCount :: Int
    } deriving (Show)

generate :: FilePath -> TypedModule -> IO ()
generate file mod =
    let defaultState = GenState
            { tmpVarCount = 0
            , typedefs = mempty
            , forwardDecls = mempty
            , program = mempty
            , genBuffer = mempty
            , operMap = M.empty
            , operCount = 0 
            } in
    case runWriter (runStateT (runExceptT (runGen mod)) defaultState) of
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

operId :: Gen Int
operId = do
    state <- get
    let count = operCount state
    put (state { operCount = count + 1 })
    return count

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
    let prog = program state
    put (state { genBuffer = mempty, program = prog <> buf })

collectBuffer :: Gen Builder
collectBuffer = do
    state <- get
    let buf = genBuffer state
    put (state { genBuffer = mempty })
    return buf

addForwardDecl :: Builder -> Gen ()
addForwardDecl txt = do
    state <- get
    let curr = forwardDecls state
    put (state { forwardDecls = curr <> txt <> "\n" })

addOperEntry :: Text -> Gen Int
addOperEntry oper = do
    id <- operId
    state <- get
    let map = operMap state
    put (state { operMap = M.insert oper id map })
    return id

-- Code generation
runGen :: TypedModule -> Gen ()
runGen decls = do
    genTopLevelDecls decls
    flushGen

    tell "// Juno compiler output\n"
    tell "\n"
    tell "// includes\n"
    tell "#include <stdlib.h>\n"
    tell "#include <stdio.h>\n"
    tell "#include <stdint.h>\n"
    tell "#include <stdbool.h>\n"
    tell "#include <math.h>\n"
    tell "#include <time.h>\n"
    tell "\n"
    tell "// typedefs\n"
    tell "typedef char unit;\n"
    tell "typedef struct string {\n"
    tell "\tchar* data;\n"
    tell "\tint len;\n"
    tell "} string;\n"
    gets typedefs >>= tell
    tell "\n"
    tell "// forward decls\n"
    gets forwardDecls >>= tell
    tell "\n"
    tell "// program\n"
    gets program >>= tell
    tell "\n"
    tell "// entry point\n"
    tell "int main() {\n"
    tell "\t_main();\n"
    tell "\treturn 0;\n"
    tell "}"

genTopLevelDecls :: TypedModule -> Gen ()
genTopLevelDecls = foldr ((*>) . genTopLevel) (return ())

genTopLevel :: TypedTopLvl -> Gen ()
genTopLevel = \case
    TLFunc (TFunc ptypes rtype) name_ params _ body -> do
        let name = if name_ == "main" then "_main" else name_
        let (pnames, _) = unzip params
        let rtypeC = convertType rtype
        let paramsText = mconcat (intersperse ", " $ [ptypeC <> " " <> fromText pname | (pname, ptypeC) <- zip pnames (map convertType ptypes)])
        let fnDecl = rtypeC <> " " <> fromText name <> "(" <> paramsText <> ")"
        outln (fnDecl <> " {")
        flushGen
        out "return "
        genExpr body
        outln ";"
        outln "}"
        flushGen

        addForwardDecl (fnDecl <> ";")

    TLOper (TFunc ptypes rtype) opdef oper params _ body -> do
        let (pnames, _) = unzip params
        let rtypeC = convertType rtype
        let paramsText = mconcat (intersperse ", " $ [ptypeC <> " " <> fromText pname | (pname, ptypeC) <- zip pnames (map convertType ptypes)])

        id <- addOperEntry oper
        let fnDecl = rtypeC <> " _operator" <> fromString (show id) <> "(" <> paramsText <> ")"

        outln (fnDecl <> " {")
        flushGen
        out "return "
        genExpr body
        outln ";"
        outln "}"
        flushGen

        addForwardDecl (fnDecl <> ";")

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
    EMatch t mexpr branches -> do
        cur <- collectBuffer

        rvar <- tmpVar
        mvar <- tmpVar
        let mvarType = typeOfExpr mexpr
        outln (convertType t <> " " <> rvar <> ";")
        out (convertType mvarType <> " " <> mvar <> " = ")
        genExpr mexpr
        outln ";"

        let branches' = takeWhileOneMore
                (\case
                    (PWild, _) -> False
                    (PVar _, _) -> False
                    _ -> True) branches
        let isExhaustive = any
                (\case
                    (PWild, _) -> True
                    (PVar _, _) -> True
                    _ -> False) branches

        genMatchBranches mvarType rvar mvar branches'

        unless isExhaustive (do
            outln " else {"
            outln "printf(\"PANIC: Non-exhaustive match expression\\n\");"
            outln "exit(-1);"
            out "}")

        outln ""
        out (cur <> rvar)
    EBinOp _ oper a b -> do
        if oper `elem` ["+", "-", "*", "/", ">", ">=", "<", "<=", "==", "!=", "||", "&&"]
            then do
                genExpr a
                out (fromText oper)
                genExpr b
            else do
                map <- gets operMap
                let id = (fromString . show) (fromJust $ M.lookup oper map)
                out ("_operator" <> id <> "(")
                genExpr a
                out ", "
                genExpr b
                out ")"
    EUnaOp _ oper expr -> do
        map <- gets operMap
        let id = (fromString . show) (fromJust $ M.lookup oper map)
        out ("_operator" <> id <> "(")
        genExpr expr
        out ")"
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

genMatchBranches :: Type -> Builder -> Builder -> [(Pattern, TypedExpr)] -> Gen ()
genMatchBranches mvarType rvar mvar [branch] = genMatchBranch mvarType rvar mvar branch
genMatchBranches mvarType rvar mvar (branch : rest) = do
    genMatchBranch mvarType rvar mvar branch
    out " else "
    genMatchBranches mvarType rvar mvar rest
genMatchBranches _ _ _ _ = return ()

genMatchBranch :: Type -> Builder -> Builder -> (Pattern, TypedExpr) -> Gen ()
genMatchBranch _ rvar mvar (PLit lit, bexpr) = do
    outln ("if (" <> mvar <> " == " <> genLit lit <> ") {")
    out (rvar <> " = ")
    genExpr bexpr
    outln ";"
    out "}"
genMatchBranch mvarType rvar mvar (PVar var, bexpr) = do
    outln "{"
    outln (convertType mvarType <> " " <> fromText var <> " = " <> mvar <> ";")
    out (rvar <> " = ")
    genExpr bexpr
    outln ";"
    out "}"
genMatchBranch _ rvar mvar (PWild, bexpr) = do
    outln "{"
    out (rvar <> " = ")
    genExpr bexpr
    outln ";"
    out "}"
    
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
    TStr -> "string"
    TChar -> "char"
    TBool -> "bool"
    TUnit -> "unit"
    TCon name [] -> fromText name
    TVar _ -> error "Parametric polymorphism not supported yet"
    other -> error (show other)

-- Utility
takeWhileOneMore :: (a -> Bool) -> [a] -> [a]
takeWhileOneMore p = foldr (\x ys -> if p x then x:ys else [x]) []
