{-# Language LambdaCase #-}
{-# Language OverloadedStrings #-}
{-# Language TemplateHaskell #-}
{-# Language FlexibleContexts #-}

module Codegen where

import Data.Text (Text, unpack, pack)
import Data.Char
import qualified Data.Text as T
import Data.Text.Lazy.Builder
import qualified Data.Text.Lazy.IO as TIO
import qualified Data.Map as M
import qualified Data.Set as S
import Data.List

import Control.Monad.Except
import Control.Monad.State

import Control.Lens

import System.IO.Temp

import Syntax
import SyntaxInfo 
import Name
import Type

import Analyzer.Substitution

import Text.Megaparsec.Pos (SourcePos, sourcePosPretty)

import Debug.Trace

type Gen = ExceptT Text (State GenState)
data GenState = GenState
    { _output :: Builder
    , _buffer :: Builder
    , _typedefs :: Builder
    , _tmpVarCount :: Int
    , _structUnionMap :: M.Map Type Int
    , _structUnionCount :: Int
    , _polyRequests :: M.Map (Name, [Type]) Int
    , _polyCount :: Int
    , _polyBaseMap :: M.Map Name TypedTopLvl
    }

makeLenses ''GenState

capriHeaders :: Builder
capriHeaders = "#include <stdio.h>\n#include <stdint.h>\n#include <stdbool.h>\ntypedef char unit;\n"

capriEntryPoint :: Builder
capriEntryPoint = "int main() {\n\tmain__main();\n\treturn 0;\n}\n"

generate :: TypedProgram -> IO [FilePath]
generate prog =
    case evalState (runExceptT (genProgram prog)) defaultGenState of
        Left err -> [] <$ print err
        Right (mods, polys) -> do
            files <- traverse (const (emptySystemTempFile "capri.c")) mods
            sequence_ [TIO.writeFile file (toLazyText (capriHeaders <> builder)) | (file, builder) <- zip files mods]
            
            entryPointFile <- emptySystemTempFile "capri.c"
            TIO.writeFile entryPointFile (toLazyText capriEntryPoint)

            polyFile <- emptySystemTempFile "capri.c"
            TIO.writeFile polyFile (toLazyText (capriHeaders <> polys))

            return (entryPointFile : polyFile : files)
    where
        defaultGenState = GenState mempty mempty mempty 0 mempty 0 mempty 0 mempty

genProgram :: TypedProgram -> Gen ([Builder], Builder)
genProgram prog = do
    modBuilders <- traverse genModule prog

    output .= mempty
    buffer .= mempty
    typedefs .= mempty

    polyReqs <- gets _polyRequests
    mapM_ genPolyRequest (M.toList polyReqs)
    tdefs <- gets _typedefs
    oput <- gets _output 
    return (modBuilders, tdefs <> oput)

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

genPolyRequest :: ((Name, [Type]), Int) -> Gen ()
genPolyRequest ((name, typs), id) = do
    baseMap <- gets _polyBaseMap
    case M.lookup name baseMap of
        Nothing -> undefined
        Just (TLFunc _ typ@(TArrow ptypes rtype) _ _ _ paramsWithAnnots _ body) -> do
            let tvars = S.toList (tvs typ)
            let subst = M.fromList (zip tvars typs)
            genFunction (apply subst ptypes) (apply subst rtype) ("_poly_" <> fromString (show id)) (map fst paramsWithAnnots) body
            flushTo output
        _ -> error "not yet implemented"

genTopLvl :: TypedTopLvl -> Gen ()
genTopLvl = \case
    tl@(TLFunc info typ@(TArrow ptypes rtype) _ isOper name paramsWithAnnots _ body) -> do
        let tvars = tvs typ
        let isPoly = not (null tvars)
        if isPoly
            then do -- Add to polybasemap
                state <- get
                put (state { _polyBaseMap = M.insert name tl (_polyBaseMap state) })
            else do
                genFunction ptypes rtype (convertName name) (map fst paramsWithAnnots) body

    TLType info _ name typeParams typ -> return ()
    _ -> throwError "genTopLvl unhandled case"

genFunction :: [Type] -> Type -> Builder -> [Name] -> TypedExpr -> Gen ()
genFunction ptypes rtype nameBuilder params bodyExpr = do
    ptypes' <- traverse convertType ptypes
    rtype' <- convertType rtype

    let paramsBuilder = mconcat (intersperse ", " $ [ptype <> " " <> convertName param | (param, ptype) <- zip params ptypes'])
    let fnDeclBuilder = rtype' <> " " <> nameBuilder <> "(" <> paramsBuilder <> ")"

    write (fnDeclBuilder <> " {\n")
    flushTo output

    write "return "
    genExpr bodyExpr

    write ";\n}\n"
    flushTo output

handlePolyRequest :: Name -> [Type] -> Gen Builder
handlePolyRequest name typs = do
    state <- get
    case M.lookup (name, typs) (_polyRequests state) of
        Just id -> return ("_poly_" <> fromString (show id))
        Nothing -> do
            count <- gets _polyCount
            put (state { _polyRequests = M.insert (name, typs) count (_polyRequests state) })
            polyCount += 1
            return ("_poly_" <> fromString (show count))

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
    EVar _ _ [] name -> write (convertName name)
    EVar _ _ typs name -> do
        toWrite <- handlePolyRequest name typs
        write toWrite
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
    EMatch synInfo t mexpr branches -> do
        curr <- collectBuffer
        var <- tmpVar
        rvar <- tmpVar
        ttype <- convertType t
        let mexprType = exprType mexpr
        mexprTypeTxt <- convertType mexprType

        write (ttype <> " " <> rvar <> ";\n")
        write (mexprTypeTxt <> " " <> var <> " = ")
        genExpr mexpr
        write ";\n"

        mapM_ (genMatchBranch rvar var mexprType) branches

        -- Panic for non-exhaustive match expressions
        write ("{\nprintf(\"%s\", \"PANIC: Non-exhaustive match expression ("
            <> (fromString . filter (/= '"') . show) (sourcePosPretty (syntaxInfoSourcePos synInfo))
            <> ")\");\nexit(-1);\n}\n")
        write (curr <> rvar)
    EBinOp _ _ oper a b -> do
        write (convertName oper <> "(")
        genExpr a
        write ", "
        genExpr b
        write ")"
    EUnaOp _ _ oper expr -> do
        write (convertName oper <> "(")
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
    EVariant _ t expr label -> do
        vtype <- convertType t
        write ("{." <> fromText label <> " = ")
        genExpr expr
        write (", ._type = " <> vtype <> fromText label <> "}")
    ERecordRestrict _ _ expr _ -> do
        genExpr expr
    where
        handleRecord var@(EVar info _ _ name) c =
            case exprType var of
                TRecord row -> let labels = getRowsLabels row in [(ERecordSelect info TUnit var label, label) | label <- labels] ++ c
                _ -> error "Attempt to extend non-record (?)"
        handleRecord (ERecordEmpty _ _) [] = []
        handleRecord (ERecordEmpty _ _) collected = collected
        handleRecord (ERecordExtend _ _ expr label rest) collected = handleRecord rest ((expr, label):collected)
        handleRecord (ERecordRestrict _ _ expr label) collected = handleRecord expr collected
        handleRecord _ collected = collected

        getRowsLabels TRowEmpty = []
        getRowsLabels (TRowExtend label _ next) = label : getRowsLabels next
        getRowsLabels _ = undefined

genMatchBranch :: Builder -> Builder -> Type -> (Pattern, TypedExpr) -> Gen ()
genMatchBranch rvar var typ (pat, expr) = do
    case pat of
        PVariant label name -> do
            typTxt <- convertType typ
            write ("if (" <> var <> "._type == " <> typTxt <> fromText label <> ") {\n")
            
            labelTypeTxt <- getTypeOfLabel label typ >>= convertType
            write (labelTypeTxt <> " " <> convertName name <> " = " <> var <> "." <> fromText label <> ";\n")

            write (rvar <> " = ")
            genExpr expr
            write ";\n} else "
        PLit lit -> do
            write ("if (" <> var <> " == " <> genLit lit <> ") {\n")
            write (rvar <> " = ")
            genExpr expr
            write ";\n} else "
        PVar name -> do
            typTxt <- convertType typ
            write "if (true) {\n"

            write (typTxt <> " " <> convertName name <> " = " <> var <> ";\n")

            write (rvar <> " = ")
            genExpr expr
            write ";\n} else "
        PWild -> do
            write "if (true) {\n"
            write (rvar <> " = ")
            genExpr expr
            write ";\n} else "
    where
        getTypeOfLabel label (TVariant row) = getTypeOfLabel label row
        getTypeOfLabel label (TRowExtend l typ rest)
            | l == label = return typ
            | otherwise = getTypeOfLabel label rest
        getTypeOfLabel label _ = error $ "getTypeOfLabel reached end (" ++ show label ++ ")"

genLit :: Lit -> Builder
genLit = \case
    LInt n -> (fromString . show) n
    LFloat n -> (fromString . show) n
    LString s -> (fromString . show . unpack) s
    LChar c -> (fromString . show) c
    LBool True -> "true"
    LBool False -> "false"
    LUnit -> "0"

{-
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
-}

convertName :: Name -> Builder
convertName name =
    let converted = convertNameToString name in
        (fromString . concat ) [txt | char <- converted, let txt = if isAlphaNum char || char == '_' then [char] else show (ord char)]
    where
        convertNameToString (Unqualified n) = T.unpack n
        convertNameToString (Qualified ns n) = T.unpack (T.intercalate "__" (ns ++ [n]))

{-
convertName :: Name -> Builder
convertName (Unqualified n) = fromText n
convertName (Qualified ns n) = fromText (T.intercalate "__" (ns ++ [n]))
-}

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
    -- TODO: clean up the repeated code here
    t@(TRecord row) -> do
        stmap <- gets _structUnionMap
        case M.lookup t stmap of
            Just id -> return ("_record_" <> fromString (show id))
            Nothing -> do
                count <- gets _structUnionCount
                structUnionMap .= M.insert t count stmap
                structUnionCount += 1

                let countBldr = fromString (show count)
                def <- convertType row
                typedefs <>= "typedef struct {char _dummy; " <> def <> "} _record_" <> countBldr <> ";\n"

                return ("_record_" <> countBldr)
    t@(TVariant row) -> do
        stmap <- gets _structUnionMap
        case M.lookup t stmap of
            Just id -> return ("_variant_" <> fromString (show id))
            Nothing -> do
                count <- gets _structUnionCount
                structUnionMap .= M.insert t count stmap
                structUnionCount += 1

                let countBldr = fromString (show count)
                def <- convertType row
                typedefs <>= "typedef struct {union {char _dummy; " <> def <> "}; int _type;} _variant_" <> countBldr <> ";\n"

                typeIds <- genVariantTypeIds countBldr row 0 []
                typedefs <>= typeIds

                return ("_variant_" <> countBldr)
    TRowEmpty -> return ""
    TRowExtend label fieldType rest -> do
        ftype <- convertType fieldType
        rtype <-
            case rest of
                TRecord row -> convertType row
                TVariant row -> convertType row
                _ -> convertType rest
        return (ftype <> " " <> fromText label <> ";" <> rtype)
    TPtr t -> (<> "*") <$> convertType t
    _ -> undefined
    where
        genVariantTypeIds _ TRowEmpty _ col = return (mconcat col)
        genVariantTypeIds cnt (TRowExtend label _ rest) id col = do
            genVariantTypeIds cnt rest (id + 1) (("const int _variant_" <> cnt <> fromText label <> " = " <> fromString (show id) <> ";\n") : col)
        genVariantTypeIds _ _ _ _ = undefined

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