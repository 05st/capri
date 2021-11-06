{-# Language OverloadedStrings #-}
{-# Language LambdaCase #-}
{-# Language TupleSections #-}
{-# Language TemplateHaskell #-}
{-# Language FlexibleContexts #-}

module Codegen (generate, flushTo) where

import Control.Monad.Writer
import Control.Monad.State
import Control.Monad.Except

import Control.Lens

import Data.Text (Text, pack, unpack)
import qualified Data.Text.Lazy.IO as TIO
import Data.Text.Lazy.Builder
import Data.Text.Lazy (toStrict)
import Data.List (intersperse, intercalate)
import Data.Foldable (traverse_)
import Data.Maybe
import qualified Data.Map as M
import qualified Data.Set as S

import Debug.Trace

import Syntax
import Type
import Name
import Substitution

type Gen = ExceptT String (StateT GenState (Writer Builder))
data GenState = GenState
    { _program :: Builder
    , _forwardDecls :: Builder
    , _typedefs :: Builder
    , _valueCons :: Builder
    , _buffer :: Builder
    , _tmpVarCount :: Int
    , _noStdLib :: Bool
    , _operMap :: M.Map Name Int
    , _operCount :: Int
    } deriving (Show)

makeLenses ''GenState

-- Run Gen
generate :: TypedProgram -> Bool -> Builder
generate prog nostl =
    let defaultGenState = GenState mempty mempty mempty mempty mempty 0 nostl mempty 0 in
    case runWriter (runStateT (runExceptT (runGen prog nostl)) defaultGenState) of
        ((Left err, _), _) -> error err
        (_, output) -> output

-- Generation
runGen :: TypedProgram -> Bool -> Gen ()
runGen prog nostl = do
    genProgram prog

    tell "// Juno Compiler Output\n\n"
    addIncludes
    addCompilerTypedefs
    addSection _forwardDecls "Forward Declarations"
    addSection _typedefs "Typedefs"
    addSection _valueCons "Value Constructors"
    addSection _program "Program"
    addEntryPoint

addIncludes :: Gen ()
addIncludes = do
    tell "// Includes\n"
    tell "#include <stdlib.h>\n"
    tell "#include <stdio.h>\n"
    tell "#include <stdint.h>\n"
    tell "#include <stdbool.h>\n"
    tell "#include <math.h>\n"
    tell "#include <time.h>\n"
    tell "\n"

addCompilerTypedefs :: Gen ()
addCompilerTypedefs = do
    tell "// Compiler Typedefs\n"
    tell "typedef char unit;\n"
    tell "\n"

addEntryPoint :: Gen ()
addEntryPoint = do
    tell "// Entry Point\n"
    tell "int main() {\n"
    tell "\tmain__main();\n"
    tell "\treturn 0;\n"
    tell "}"

addSection :: (GenState -> Builder) -> Builder -> Gen ()
addSection proj label = do
    tell ("// " <> label <> "\n")
    gets proj >>= tell
    tell "\n"

genProgram :: TypedProgram -> Gen ()
genProgram = traverse_ genModule

genModule :: TypedModule -> Gen ()
genModule (Module _ name _ topLvls _) = do
    out "// "
    outln (mconcat $ intersperse "::" $ map fromText name)
    flushTo program
    traverse_ genTopLvl topLvls
    flushTo program

genTopLvl :: TypedTopLvl -> Gen ()
genTopLvl = \case
    TLFunc t _ name params _ body -> genFunction False t name params body

    TLOper t _ oper params _ body -> genFunction True t oper params body

    TLType _ name tparams valueCons -> do
        let typeName' = "_t_" <> convertName name
        let (conNames, conTypes) = unzip valueCons
        let conNames' = map convertName conNames

        traverse_ (genTypeVariant typeName') valueCons

        typedefs <>=
            "typedef union " <> typeName' <> "Variants {\n"
            <> (if null conNames' then "\tchar dummy;\n" else "")
            <> mconcat ["\t" <> typeName' <> conName <> " " <> conName <> ";\n" | conName <- conNames']
            <> "} " <> typeName' <> "Variants;\n"

        typedefs <>=
            "typedef enum " <> typeName' <> "Tag {\n"
            <> mconcat ["\t" <> conName <> "Tag,\n" | conName <- conNames']
            <> "} " <> typeName' <> "Tag;\n"

        typedefs <>=
            "typedef struct " <> typeName' <> " {\n"
            <> "\t" <> typeName' <> "Tag tag;\n"
            <> "\t" <> typeName' <> "Variants data;\n"
            <> "} " <> typeName' <> ";\n"

        forwardDecls <>= ("typedef struct " <> typeName' <> " " <> typeName' <> ";\n")

    TLStruct _ name tparams fields -> do
        let name' = "_t_" <> convertName name
        let (labels, types) = unzip fields
        let labels' = map fromText labels

        typedefs <>=
            "typedef struct " <> name' <> " {\n"
            <> mconcat ["\t" <> convertType typ <> " " <> label <> ";\n" | (typ, label) <- zip types labels']
            <> "} " <> name' <> ";\n"

        forwardDecls <>= ("typedef struct " <> name' <> " " <> name' <> ";\n")

    TLExtern {} -> return ()
    where
        genTypeVariant typeName (conName, conTypes) = do
            let conTypes' = map convertType conTypes
            let conName' = convertName conName
            let tIds = map (fromString . show) [0..]
            let conParamList = [conType <> " _" <> tId | (conType, tId) <- zip conTypes' tIds]

            typedefs <>=
                "typedef struct " <> typeName <> conName' <> " {\n"
                <> (if null conTypes then "\tchar dummy;\n" else "")
                <> mconcat (map (\p -> "\t" <> p <> ";\n") conParamList)
                <> "} " <> typeName <> conName' <> ";\n"

            valueCons <>=
                "inline static " <> typeName <> " " <> conName' <> "(" <> mconcat (intersperse ", " conParamList) <> ") {\n"
                <> "\t" <> typeName <> " result;\n"
                <> "\t" <> "result.tag = " <> conName' <> "Tag;\n"
                <> mconcat ["\tresult.data." <> conName' <> "._" <> tId <> " = _" <> tId <> ";\n" | tId <- take (length conTypes) tIds]
                <> "\treturn result;\n"
                <> "}\n"

            typedefs <>=
                mconcat ["typedef " <> conType <> " " <> conName' <> "_" <> tId <> ";\n" | (conType, tId) <- zip conTypes' tIds]

        genFunction isOper (TFunc ptypes rtype) name params_ body = do
            let (params, _) = unzip params_
            let ptypes' = map convertType ptypes
            let rtype' = convertType rtype
            let name' = convertName name

            let paramsBuilder = mconcat (intersperse ", " $ [ptype <> " " <> fromText param | (param, ptype) <- zip params ptypes'])
            fnDecl <- if isOper
                then addOperEntry name >>= \id -> return (rtype' <> " _op_" <> fromString (show id) <> "(" <> paramsBuilder <> ")")
                else return (rtype' <> " " <> name' <> "(" <> paramsBuilder <> ")")

            outln (fnDecl <> " {")
            flushTo program

            out "return "
            genExpr body

            outln ";"
            outln "}"
            flushTo program

            forwardDecls <>= fnDecl <> ";\n"
        genFunction _ _ _ _ _ = throwError "genFunction failed"

genDecl :: TypedDecl -> Gen ()
genDecl = \case
    DVar (TArray t len) _ _ name _ (EArray _ _ exprs) -> do
        out (convertType t <> " " <> fromText name <> "[" <> (fromString . show $ len) <> "] = {")
        sequence_ (intersperse (out ", ") (map genExpr exprs))
        outln "};"
        flushTo program
    DVar t _ _ name _ expr -> do
        out (convertType t <> " " <> fromText name <> " = ")
        genExpr expr
        outln ";"
        flushTo program
    DStmt s -> genStmt s

genStmt :: TypedStmt -> Gen ()
genStmt = \case
    SRet expr -> do
        out "return "
        genExpr expr
        outln ";"
        flushTo program
    SWhile _ cond body -> do
        out "while ("
        genExpr cond
        outln ") {"
        flushTo program
        genExpr body
        outln ";"
        outln "}"
        flushTo program
    SExpr expr -> do
        genExpr expr
        outln ";"
        flushTo program

genExpr :: TypedExpr -> Gen ()
genExpr = \case
    ELit _ _ lit -> out (genLit lit)

    EVar _ _ typs name -> out (convertName name)

    EAssign _ _ l r -> do
        genExpr l
        out " = "
        genExpr r

    EBlock _ _ decls res -> do
        curr <- collectBuffer
        flushTo program
        traverse_ genDecl decls
        flushTo program
        out curr
        genExpr res

    EIf _ _ cond a b -> do
        out "("
        genExpr cond
        out ") ? ("
        genExpr a
        out ") : ("
        genExpr b
        out ")"

    EMatch t _ mexpr branches -> do
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

    EBinOp _ _ oper a b -> do
        if extractName oper `elem` ["+", "-", "*", "/", ">", ">=", "<", "<=", "==", "!=", "||", "&&"]
            then do
                out "("
                genExpr a
                out (fromText $ extractName oper)
                genExpr b
                out ")"
            else do
                opMap <- gets _operMap
                let id = (fromString . show) (fromJust $ M.lookup oper opMap)
                out ("_op_" <> id <> "(")
                genExpr a
                out ", "
                genExpr b
                out ")"

    EUnaOp _ _ oper expr -> do
        opMap <- gets _operMap
        let id = (fromString . show) (fromJust $ M.lookup oper opMap)
        out ("_op_" <> id <> "(")
        genExpr expr
        out ")"

    EClosure {} -> throwError "no closures yet"

    ECall _ _ fnexpr args -> do
        genExpr fnexpr
        out "("
        sequence_ (intersperse (out ", ") (map genExpr args))
        out ")"

    ECast _ _ targ expr -> do 
        out ("(" <> convertType targ <> ")")
        genExpr expr

    EDeref _ _ expr -> do
        out "*"
        genExpr expr

    ERef _ _ expr -> do
        out "&"
        genExpr expr

    ESizeof _ _ arg -> do
        out "sizeof("
        case arg of
            Left typ -> do
                out (convertType typ)
            Right expr -> genExpr expr
        out ")"

    EArray (TArray t len) _ exprs -> do
        cur <- collectBuffer
        tvar <- tmpVar
        out (convertType t <> " " <> tvar <> "[" <> (fromString . show $ len) <> "] = ") 
        out "{"
        sequence_ (intersperse (out ", ") (map genExpr exprs))
        outln "};"
        out (cur <> tvar)

    EArray {} -> throwError "Analyzer error"

    EIndex t _ expr idx -> do
        genExpr expr
        out ("[" <> (fromString . show $ idx) <> "]")

    EStruct _ _ structName fields -> do
        out ("(_t_" <> convertName structName <> "){")
        sequence_ (intersperse (out ", ") [out ("." <> fromText label <> " = ") *> genExpr expr | (label, expr) <- fields])
        out "}"

    EAccess t _ expr label -> do
        genExpr expr
        out ("." <> fromText label)

genLit :: Lit -> Builder
genLit = \case
    LInt n -> (fromString . show) n
    LFloat n -> (fromString . show) n
    LString s -> (fromString . show . unpack) s
    LChar c -> "'" <> singleton c <> "'"
    LBool True -> "true"
    LBool False -> "false"
    LUnit -> "0"

-- Match branches
-- TODO: Rewrite all of this
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
    flushTo program
    out (rvar <> " = ")
    genExpr bexpr
    outln ";"
    out "}"
genMatchBranch mvarType rvar mvar (PVar var, bexpr) = do
    outln "{"
    flushTo program
    outln (convertType mvarType <> " " <> fromText var <> " = " <> mvar <> ";")
    out (rvar <> " = ")
    genExpr bexpr
    outln ";"
    out "}"
genMatchBranch _ rvar mvar (PWild, bexpr) = do
    outln "{"
    flushTo program
    out (rvar <> " = ")
    genExpr bexpr
    outln ";"
    out "}"
genMatchBranch mvarType rvar mvar (PCon conName binds, bexpr) = do
    let conName' = convertName conName
    let binds' = map fromText binds
    let tIds = map (fromString . show) [0..]
    outln ("if (" <> mvar <> ".tag == " <> convertName conName <> "Tag) {")
    out $ mconcat [conName' <> "_" <> tId <> " " <> bind <> " = " <> mvar <> ".data." <> conName' <> "._" <> tId <> ";\n" | (bind, tId) <- zip binds' tIds, bind /= "_"]
    flushTo program
    out (rvar <> " = ")
    genExpr bexpr
    outln ";"
    out "}"

-- Capri types to C types
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
    TCon name [] -> "_t_" <> convertName name
    TArray t _ -> convertType t <> singleton '*'
    other -> error (show other)

-- Name to builder
convertName :: Name -> Builder
convertName (Qualified quals) = fromString (intercalate "__" (map unpack quals))
convertName (Unqualified name) = fromText name

-- Utility
tmpVar :: Gen Builder
tmpVar = do
    count <- gets _tmpVarCount
    tmpVarCount += 1
    return (fromText . pack $ names !! count)
    where
        names = map ('_' :) $ [1..] >>= flip replicateM ['a'..'z']

out :: Builder -> Gen ()
out str = do
    state <- get
    put (state { _buffer = _buffer state <> str })

outln :: Builder -> Gen ()
outln = out . (<> "\n")

flushTo :: ASetter GenState GenState Builder Builder -> Gen ()
flushTo l = do
    toFlush <- gets _buffer
    l <>= toFlush
    buffer .= mempty

collectBuffer :: Gen Builder
collectBuffer = do
    buf <- gets _buffer
    buffer .= mempty
    return buf

addOperEntry :: Name -> Gen Int
addOperEntry oper = do
    count <- gets _operCount
    operCount += 1
    opMap <- gets _operMap
    operMap .= M.insert oper count opMap
    return count

takeWhileOneMore :: (a -> Bool) -> [a] -> [a]
takeWhileOneMore p = foldr (\x ys -> if p x then x:ys else [x]) []
