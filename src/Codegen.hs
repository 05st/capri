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
    { tmpVarCount :: Int
    , typedefs :: Builder
    , valueCons :: Builder
    , forwardDecls :: Builder
    , program :: Builder
    , genBuffer :: Builder
    , operMap :: M.Map Name Int
    , operCount :: Int
    , noStdLib :: Bool
    , macros :: Builder
    , inMacro :: Bool
    , polyInits :: Builder
    , polys :: S.Set Name
    } deriving (Show)

generate :: FilePath -> TypedProgram -> Bool -> IO ()
generate file prog nostl =
    let defaultState = GenState
            { tmpVarCount = 0
            , typedefs = mempty
            , valueCons = mempty
            , forwardDecls = mempty
            , program = mempty
            , genBuffer = mempty
            , operMap = M.empty
            , operCount = 0 
            , noStdLib = nostl
            , macros = mempty
            , inMacro = False
            , polyInits = mempty
            , polys = S.empty
            } in
    case runWriter (runStateT (runExceptT (runGen prog)) defaultState) of
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
outln txt = do
    inMacro <- gets inMacro
    out (txt <> if inMacro then "\\\n" else "\n")

outBefore :: Builder -> Gen ()
outBefore txt = do
    state <- get
    put (state { genBuffer = txt <> genBuffer state  })

flushGen :: Gen ()
flushGen = do
    state <- get
    let buf = genBuffer state
    inMacro <- gets inMacro
    if inMacro
        then let macs = macros state
             in put (state { genBuffer = mempty, macros = macs <> buf })
        else let prog = program state
             in put (state { genBuffer = mempty, program = prog <> buf })

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

addOperEntry :: Name -> Gen Int
addOperEntry oper = do
    id <- operId
    state <- get
    let map = operMap state
    put (state { operMap = M.insert oper id map })
    return id

addTypedef :: Builder -> Gen ()
addTypedef txt = do
    state <- get
    let curr = typedefs state
    put (state { typedefs = curr <> txt })

addValueCon :: Builder -> Gen ()
addValueCon txt = do
    state <- get
    let curr = valueCons state
    put (state { valueCons = curr <> txt })

setInMacro :: Bool -> Gen ()
setInMacro val = do
    state <- get
    put (state { inMacro = val })

addPolyInit :: Builder -> Gen ()
addPolyInit txt = do
    state <- get
    let curr = polyInits state
    put (state { polyInits = curr <> txt <> "\n" })

addPoly :: Name -> Gen ()
addPoly name = do
    state <- get
    let curr = polys state
    put (state { polys = S.insert name curr })

-- Code generation
runGen :: TypedProgram -> Gen ()
runGen mods = do
    traverse_ genModule mods
    flushGen

    tell "// Juno compiler output\n"
    tell "\n"
    nostl <- gets noStdLib
    unless nostl (do
        tell "// includes\n"
        tell "#include <stdlib.h>\n"
        tell "#include <stdio.h>\n"
        tell "#include <stdint.h>\n"
        tell "#include <stdbool.h>\n"
        tell "#include <math.h>\n"
        tell "#include <time.h>\n"
        tell "\n")
    tell "// compiler typedefs\n"
    tell "typedef char unit;\n"
    tell "typedef struct string {\n"
    tell "\tchar* data;\n"
    tell "\tint len;\n"
    tell "} string;\n"
    tell "\n"
    tell "// forward decls\n"
    gets forwardDecls >>= tell
    tell "\n"
    tell "// typedefs\n"
    gets typedefs >>= tell
    tell "\n"
    tell "// value constructors\n"
    gets valueCons >>= tell
    tell "\n"
    tell "// macros\n"
    gets macros >>= tell
    tell "\n"
    tell "// poly inits\n"
    gets polyInits >>= tell
    tell "\n"
    tell "// program\n"
    gets program >>= tell
    tell "\n"
    tell "// entry point\n"
    tell "int main() {\n"
    tell "\tmain__main();\n"
    tell "\treturn 0;\n"
    tell "}"

genModule :: TypedModule -> Gen ()
genModule (Module _ name _ topLvls _) = do
    outln ("// " <> convertName (Qualified name))
    foldr ((*>) . genTopLevel) (return ()) topLvls

genTopLevel :: TypedTopLvl -> Gen ()
genTopLevel = \case
    TLFunc t _ name_ params _ body -> genFunction False t name_ params body
    TLOper t _ oper_ params _ body -> genFunction True t oper_ params body

    TLType _ typeName tparams_ valueCons -> do
        let typeName' = "_t_" <> convertName typeName
        let (conNames, conTypes) = unzip valueCons
        let conNames' = map convertName conNames

        traverse_ (genTypeVariant typeName') valueCons

        addTypedef $
            "typedef union " <> typeName' <> "Variants {\n"
            <> (if null conNames' then "\tchar dummy;\n" else "")
            <> mconcat ["\t" <> typeName' <> conName <> " " <> conName <> ";\n" | conName <- conNames']
            <> "} " <> typeName' <> "Variants;\n"

        addTypedef $
            "typedef enum " <> typeName' <> "Tag {\n"
            <> mconcat ["\t" <> conName <> "Tag,\n" | conName <- conNames']
            <> "} " <> typeName' <> "Tag;\n"

        addTypedef $
            "typedef struct " <> typeName' <> " {\n"
            <> "\t" <> typeName' <> "Tag tag;\n"
            <> "\t" <> typeName' <> "Variants data;\n"
            <> "} " <> typeName' <> ";\n"

        addForwardDecl ("typedef struct " <> typeName' <> " " <> typeName' <> ";")

    TLStruct _ structName tparams fields -> do
        let typeName' = "_t_" <> convertName structName
        let (labels, types) = unzip fields
        let labels' = map fromText labels

        addTypedef $
            "typedef struct " <> typeName' <> " {\n"
            <> mconcat ["\t" <> convertType typ <> " " <> label <> ";\n" | (typ, label) <- zip types labels']
            <> "} " <> typeName' <> ";\n"

        addForwardDecl ("typedef struct " <> typeName' <> " " <> typeName' <> ";")

    TLExtern {} -> return ()

genFunction :: Bool -> Type -> Name -> Params -> TypedExpr -> Gen ()
genFunction isOper t@(TFunc ptypes rtype) name_ params body = do
    let tvars = map TVar (S.toList $ tvs t)
    let isPoly = not (null tvars)
    name <- if isPoly
        then do
            addPoly name_
            flushGen
            setInMacro True
            let name' = convertName name_
            outln ("#define _poly_" <> name' <> "(__id, " <> mconcat (intersperse ", " (map convertType tvars)) <> ")")
            return (name' <> "##__id ")
        else return (convertName name_)

    let (pnames, _) = unzip params
    let rtypeC = convertType rtype
    let ptypes' = map convertType ptypes
    let paramsText = mconcat (intersperse ", " $ [ptypeC <> " " <> fromText pname | (pname, ptypeC) <- zip pnames ptypes'])

    fnDecl <- if isOper
        then addOperEntry name_ >>= \id -> return (rtypeC <> " _op_" <> fromString (show id) <> "(" <> paramsText <> ")")
        else return (rtypeC <> " " <> name <> "(" <> paramsText <> ")")

    outln (fnDecl <> " {")
    flushGen
    out "return "
    genExpr body
    outln ";"
    outln "}"
    flushGen

    unless isPoly (addForwardDecl (fnDecl <> ";"))
    setInMacro False
genFunction _ _ _ _ _ = throwError "genFunction failed (?)"

genTypeVariant :: Builder -> (Name, [Type]) -> Gen ()
genTypeVariant typeName (conName, conTypes) = do
    let conTypes' = map convertType conTypes
    let conName' = convertName conName
    let tIds = map (fromString . show) [0..]
    let conParamList = [conType <> " _" <> tId | (conType, tId) <- zip conTypes' tIds]

    addTypedef $
        "typedef struct " <> typeName <> conName' <> " {\n"
        <> (if null conTypes then "\tchar dummy;\n" else "")
        <> mconcat (map (\p -> "\t" <> p <> ";\n") conParamList)
        <> "} " <> typeName <> conName' <> ";\n"

    addValueCon $
        "inline static " <> typeName <> " " <> conName' <> "(" <> mconcat (intersperse ", " conParamList) <> ") {\n"
        <> "\t" <> typeName <> " result;\n"
        <> "\t" <> "result.tag = " <> conName' <> "Tag;\n"
        <> mconcat ["\tresult.data." <> conName' <> "._" <> tId <> " = _" <> tId <> ";\n" | tId <- take (length conTypes) tIds]
        <> "\treturn result;\n"
        <> "}\n"

    addTypedef $
        mconcat ["typedef " <> conType <> " " <> conName' <> "_" <> tId <> ";\n" | (conType, tId) <- zip conTypes' tIds]

genDecl :: TypedDecl -> Gen ()
genDecl = \case
    DVar (TArray t len) _ _ name _ (EArray _ _ exprs) -> do
        out (convertType t <> " " <> fromText name <> "[" <> (fromString . show $ len) <> "] = {")
        sequence_ (intersperse (out ", ") (map genExpr exprs))
        outln "};"
        flushGen
    DVar t _ _ name _ expr -> do
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
    SWhile _ cond body -> do
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
    ELit _ _ lit -> out (genLit lit)
    EVar t _ ityps name -> do
        let name' = convertName name
        polys <- gets polys
        if name `S.member` polys
            then do
                id <- tmpVar
                addPolyInit ("_poly_" <> name' <> "(" <> id <> ", "<> mconcat (intersperse ", " (map convertType ityps)) <> ");")
                out (name' <> id)
            else out name'
    EAssign _ _ l r -> do
        genExpr l
        out " = "
        genExpr r
    EBlock t _ decls res -> do
        cur <- collectBuffer
        flushGen
        traverse_ genDecl decls
        flushGen
        out cur
        genExpr res
    EIf _ _ cond texpr fexpr -> do
        out "("
        genExpr cond
        out ") ? ("
        genExpr texpr
        out ") : ("
        genExpr fexpr
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
                map <- gets operMap
                let id = (fromString . show) (fromJust $ M.lookup oper map)
                out ("_op_" <> id <> "(")
                genExpr a
                out ", "
                genExpr b
                out ")"
    EUnaOp _ _ oper expr -> do
        map <- gets operMap
        let id = (fromString . show) (fromJust $ M.lookup oper map)
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
    flushGen
    out (rvar <> " = ")
    genExpr bexpr
    outln ";"
    out "}"
genMatchBranch mvarType rvar mvar (PVar var, bexpr) = do
    outln "{"
    flushGen
    outln (convertType mvarType <> " " <> fromText var <> " = " <> mvar <> ";")
    out (rvar <> " = ")
    genExpr bexpr
    outln ";"
    out "}"
genMatchBranch _ rvar mvar (PWild, bexpr) = do
    outln "{"
    flushGen
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
    flushGen
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
    TStr -> "char*" --"string"
    TChar -> "char"
    TBool -> "bool"
    TUnit -> "unit"
    TCon name [] -> "_t_" <> convertName name
    a@(TArray t _) -> convertType t <> singleton '*'
        {-
        tstr <- convertType t
        return (tstr <> "*")
        addStaticArrayType a
        tstr <- convertType t
        return ("array_" <> tstr <> "_" <> (fromString . show $ l))
        -}

    TVar (TV v) -> fromText v
    other -> error (show other)

-- Qualified/Unqualified names to builder
convertName :: Name -> Builder
convertName (Qualified quals) = fromString (intercalate "__" (map unpack quals))
convertName (Unqualified name) = fromText name

-- Utility
takeWhileOneMore :: (a -> Bool) -> [a] -> [a]
takeWhileOneMore p = foldr (\x ys -> if p x then x:ys else [x]) []
