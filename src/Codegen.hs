{-# Language LambdaCase #-}
{-# Language OverloadedStrings #-}
{-# Language TemplateHaskell #-}
{-# Language FlexibleContexts #-}

module Codegen where

import Data.Text (Text, unpack, pack)
import qualified Data.Text as T
import Data.Text.Lazy.Builder
import qualified Data.Text.Lazy.IO as TIO
import qualified Data.Map as M
import Data.List

import Control.Monad.Except
import Control.Monad.State

import Control.Lens

import System.IO.Temp

import Syntax
import Name
import Type

type Gen = ExceptT Text (State GenState)
data GenState = GenState
    { _output :: Builder
    , _buffer :: Builder
    , _typedefs :: Builder
    , _tmpVarCount :: Int
    , _structMap :: M.Map Type Int
    , _operMap :: M.Map Name Int
    , _operCount :: Int
    , _structCount :: Int
    }

makeLenses ''GenState

capriHeaders :: Builder
capriHeaders = "#include <stdio.h>\n#include <stdint.h>\n#include <stdbool.h>\n"

generate :: TypedProgram -> IO ()
generate prog =
    case evalState (runExceptT (genProgram prog)) defaultGenState of
        Left err -> print err
        Right mods -> do
            files <- traverse (const (emptySystemTempFile "capri.c")) mods
            sequence_ [TIO.writeFile file (toLazyText (capriHeaders <> builder)) | (file, builder) <- zip files mods]
            print files
    where
        defaultGenState = GenState mempty mempty mempty 0 mempty mempty 0 0

genProgram :: TypedProgram -> Gen [Builder]
genProgram = traverse genModule

genModule :: TypedModule -> Gen Builder
genModule mod = do
    output .= mempty
    buffer .= mempty
    typedefs .= mempty

    write "// "
    write $ mconcat (intersperse "::" (map fromText (modPath mod))) <> "::" <> fromText (modName mod) <> "\n"
    flushTo output

    mapM_ genTopLvl (modTopLvls mod)

    _typedefs' <- gets _typedefs
    _output' <- gets _output
    output .= _typedefs' <> _output'

    gets _output

genTopLvl :: TypedTopLvl -> Gen ()
genTopLvl = \case
    TLFunc info (TArrow ptypes rtype) _ isOper name paramsWithAnnots _ body -> do
        nameBuilder <- if isOper then handleOper name else return (convertName name)
        let (params, _) = unzip paramsWithAnnots
        ptypes' <- traverse convertType ptypes
        rtype' <- convertType rtype

        let paramsBuilder = mconcat (intersperse ", " $ [ptype <> " " <> fromText param | (param, ptype) <- zip params ptypes'])
        let fnDeclBuilder = rtype' <> " " <> nameBuilder <> "(" <> paramsBuilder <> ")"

        write (fnDeclBuilder <> " {\n")
        flushTo output

        write "return "
        genExpr body

        write ";\n}\n"
        flushTo output
    TLType info _ name typeParams typ -> return ()
    _ -> throwError "genTopLvl unhandled case"

genDecl :: TypedDecl -> Gen ()
genDecl = \case
    DVar info _ name _ expr -> do
        etype <- convertType (exprType expr)
        write $ etype <> " " <> convertName name <> " = "
        genExpr expr
        write ";\n"
        flushTo output
    DStmt s -> genStmt s

genStmt :: TypedStmt -> Gen ()
genStmt = \case
    SRet expr -> do
        write "return "
        genExpr expr
        write ";\n"
        flushTo output
    SWhile info cond body -> do
        write "while ("
        genExpr cond
        write ") {\n"
        flushTo output
        genExpr body
        write ";\n"
        write "}\n"
        flushTo output
    SExpr expr -> do
        genExpr expr
        write ";\n"
        flushTo output

genExpr :: TypedExpr -> Gen ()
genExpr = \case
    ELit _ _ lit -> write (genLit lit)
    EVar _ _ typs name -> write (convertName name)
    EAssign _ _ l r -> do
        genExpr l
        write " = "
        genExpr r
    EBlock _ _ decls res -> do
        curr <- collectBuffer
        flushTo output
        mapM_ genDecl decls
        flushTo output
        write curr
        genExpr res
    EIf _ t cond a b -> do
        curr <- collectBuffer
        var <- tmpVar
        ttype <- convertType t
        write (ttype <> " " <> var <> ";\n")
        write "if ("
        genExpr cond
        write ") {\n"
        flushTo output
        write (var <> " = ")
        genExpr a
        write ";\n} else {\n"
        flushTo output
        write (var <> " = ")
        genExpr b
        write ";\n}\n"
        write (curr <> var)
    EMatch _ t mexpr branches -> throwError "no match exprs yet"
    EBinOp _ _ oper a b -> do
        name <- handleOper oper
        write "("
        genExpr a
        write ", "
        genExpr b
        write ")"
    EUnaOp _ _ oper expr -> do
        name <- handleOper oper
        write "("
        genExpr expr
        write ")"
    EClosure {} -> throwError "no closures yet"
    ECall _ _ fn args -> do
        genExpr fn
        write "("
        sequence_ (intersperse (write ", ") (map genExpr args))
        write ")"
    ECast _ targ expr -> do
        targtype <- convertType targ
        write ("(" <> targtype <> ")")
        genExpr expr
    ERecordEmpty _ _ -> write "{}"
    ERecordSelect _ _ record label -> do
        write "("
        genExpr record
        write ")."
        write (fromText label)
    r@(ERecordExtend _ _ expr label rest) -> do
        curr <- collectBuffer
        rtype <- convertType (exprType r)
        tvar <- tmpVar
        typ <- convertType (exprType r)
        write (typ <> " " <> tvar <> ";\n")

        let exprLabels = handleRecord r []
        sequence_ [write (tvar <> "." <> fromText label <> " = ") *> genExpr expr *> write ";\n" | (expr, label) <- exprLabels]

        write (curr <> tvar)
    _ -> undefined
    where
        handleRecord (ERecordEmpty _ _) [] = []
        handleRecord (ERecordEmpty _ _) collected = collected
        handleRecord (ERecordExtend _ _ expr label rest) collected = handleRecord rest ((expr, label):collected)
        handleRecord (ERecordRestrict _ _ expr label) collected = handleRecord expr collected
        handleRecord _ collected = collected

genLit :: Lit -> Builder
genLit = \case
    LInt n -> (fromString . show) n
    LFloat n -> (fromString . show) n
    LString s -> (fromString . show . unpack) s
    LChar c -> (fromString . show) c
    LBool True -> "true"
    LBool False -> "false"
    LUnit -> "0"

handleOper :: Name -> Gen Builder
handleOper name = do
    map <- gets _operMap
    id <- case M.lookup name map of
        Nothing -> do
            count <- gets _operCount
            operMap .= M.insert name count map
            operCount += 1
            return count
        Just id -> return id
    return ("_op_" <> fromString (show id))
        
convertName :: Name -> Builder
convertName (Unqualified n) = fromText n
convertName (Qualified ns n) = fromText (T.intercalate "__" (ns ++ [n]))

convertType :: Type -> Gen Builder
convertType = \case
    TInt8 -> return "int8_t"
    TInt16 -> return "int16_t"
    TInt32 -> return "int32_t"
    TInt64 -> return "int64_t"
    TUInt8 -> return "uint8_t"
    TUInt16 -> return "uint16_t"
    TUInt32 -> return "uint32_t"
    TUInt64 -> return "uint64_t"
    TFloat32 -> return "float"
    TFloat64 -> return "double"
    TString -> return "char*"
    TChar -> return "char"
    TBool -> return "bool"
    TUnit -> return "unit"
    TConst name -> return ("_t_" <> convertName name)
    TVar (TV x) -> return (fromText x)
    t@(TRecord row) -> do
        stmap <- gets _structMap
        case M.lookup t stmap of
            Just id -> return ("_struct_" <> fromString (show id))
            Nothing -> do
                count <- gets _structCount
                structMap .= M.insert t count stmap
                structCount += 1

                let countBldr = fromString (show count)
                def <- convertType row
                typedefs <>= "typedef struct {" <> def <> "} _struct_" <> countBldr <> ";\n"

                return ("_struct_" <> fromString (show count))
    TVariant row -> error "variants not supported yet"
    TRowEmpty -> return ""
    TRowExtend label fieldType rest -> do
        ftype <- convertType fieldType
        rtype <- convertType rest
        return (ftype <> " " <> fromText label <> ";" <> rtype)
    _ -> undefined

-- Utility
write :: Builder -> Gen ()
write txt = do
    buffer <>= txt

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

tmpVar :: Gen Builder
tmpVar = do
    count <- gets _tmpVarCount
    tmpVarCount += 1
    return (fromText . pack $ names !! count)
    where
        names = map ('_' :) $ [1..] >>= flip replicateM ['a'..'z']