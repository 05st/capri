{-# Language OverloadedStrings #-}
{-# Language LambdaCase #-}
{-# Language FlexibleContexts #-}
{-# Language TupleSections #-}

module Analyzer.Resolver where

import Data.Text (Text, pack, unpack, take, toUpper)
import Data.Maybe
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Except
import qualified Data.Map as M
import qualified Data.Set as S

import Analyzer.AnalyzerError
import Syntax
import SyntaxInfo
import Type
import Name

import Debug.Trace

type Resolve = ExceptT AnalyzerError (ReaderT [Text] (State ResolveState))
data ResolveState = ResolveState
    { nameSet :: S.Set Name
    , pubMap :: M.Map Name Bool
    , typeAliasMap :: M.Map Name Type
    , enumMap :: M.Map Name [Text]
    , curMod :: Maybe UntypedModule 
    , extraSet :: S.Set Name -- probably not the best way to check for duplicate top levels but works
    , externSet :: S.Set Text
    , tmpScopeCount :: Int
    , importsMap :: M.Map [Text] [Import]
    } deriving (Show)

resolveProgram :: UntypedProgram -> Either AnalyzerError UntypedProgram
resolveProgram prog = evalState (runReaderT (runExceptT (traverse resolveModule prog)) []) initResolveState
    where
        initResolveState = ResolveState {
            nameSet = initNameSet,
            pubMap = initPubMap,
            typeAliasMap = initTypeAliasMap, -- typeAliasMap/enumMap also serve to distinguish between types of toplevel definitions
            enumMap = initEnumMap,           -- to prevent from using a function as the name of a type, since nameSet doesn't distinguish
            curMod = Nothing,
            extraSet = S.empty,
            externSet = S.empty,
            tmpScopeCount = 0,
            importsMap = initImportsMap
        }
        initPubMap = M.fromList $ concatMap (\mod -> concatMap (topLvlEntry mod) (modTopLvls mod)) prog
        topLvlEntry mod tl = [(head $ fullNameHelper mod tl, isTopLvlPub tl)]
        initImportsMap = M.fromList $ map (\mod -> (getModFullName mod, modImports mod)) prog
        initNameSet = S.fromList $ concatMap (\mod -> concatMap (fullNameHelper mod) (modTopLvls mod)) prog
        initTypeAliasMap = M.fromList $ concatMap (\mod -> concatMap (typeAliasEntry mod) (modTopLvls mod)) prog
        initEnumMap = M.fromList $ concatMap (\mod -> concatMap (enumEntry mod) (modTopLvls mod)) prog
        -- ^ contains all of the top level declarations of each module, this is so mutual recursion works
        
        fullNameHelper mod (TLFunc _ _ _ _ (Unqualified name) _ _ _) = [Qualified (getModFullName mod) name]
        fullNameHelper mod (TLType _ _ (Unqualified name) _ _) = [Qualified (getModFullName mod) name]
        fullNameHelper mod (TLEnum _ _ (Unqualified name) _ _) = [Qualified (getModFullName mod) name]
        fullNameHelper _ (TLFunc _ _ _ _ name _ _ _) = [name]
        fullNameHelper _ (TLType _ _ name _ _) = [name]
        fullNameHelper _ (TLEnum _ _ name _ _) = [name]

        typeAliasEntry mod tl@(TLType _ _ _ _ typ) = [(head $ fullNameHelper mod tl, typ)]
        typeAliasEntry _ _ = []

        enumEntry mod tl@(TLEnum _ _ _ _ variants) = [(head $ fullNameHelper mod tl, map fst variants)]
        enumEntry _ _ = []

resolveModule :: UntypedModule -> Resolve UntypedModule
resolveModule mod = do
    state <- get
    put (state { curMod = Just mod, externSet = S.fromList (map (\(n, _, _) -> n) (modExterns mod)) })
    resolvedTopLvls <- traverse resolveTopLvl (modTopLvls mod)
    return (mod { modTopLvls = resolvedTopLvls })

resolveTopLvl :: UntypedTopLvl -> Resolve UntypedTopLvl
resolveTopLvl = \case
    TLFunc info () isPub isOper name@(Unqualified unqual) params typeAnnot expr -> do
        fullName <- topLvlDefinition info unqual
        typeAnnot' <- resolveTypeAnnot info typeAnnot

        local (++ [unqual]) (do
            let (pnames, pannots) = unzip params
            pnames' <- traverse (insertNameToSet . extractName) pnames
            pannots' <- traverse (resolveTypeAnnot info) pannots
            expr' <- resolveExpr expr
            return (TLFunc info () isPub isOper fullName (zip pnames' pannots') typeAnnot' expr'))

    TLType info isPub name@(Unqualified unqual) tvars typ -> do
        name' <- topLvlDefinition info unqual
        TLType info isPub name' tvars <$> resolveType info typ

    TLEnum info isPub name@(Unqualified unqual) tvars variants -> do
        name' <- topLvlDefinition info unqual
        TLEnum info isPub name' tvars <$> traverse (resolveEnumVariant info) variants

    _ -> undefined

    where
        -- Adds to the extraSet which is used for checking redefinitions
        topLvlDefinition info unqual = do
            eset <- gets extraSet
            scope <- gets (getModFullName . fromJust . curMod) -- scope <- prependModulePath []
            let fullName = Qualified scope unqual
            when (fullName `S.member` eset)
                $ throwError (GenericAnalyzerError info ("Redefinition of " ++ show fullName))
            state <- get
            put (state { extraSet = S.insert fullName eset })
            return fullName

        resolveEnumVariant info (name, types) = do
            resolvedTypes <- traverse (resolveType info) types
            return (name, resolvedTypes)

resolveDecl :: UntypedDecl -> Resolve UntypedDecl
resolveDecl = \case
    DStmt stmt -> DStmt <$> resolveStmt stmt
    DVar info isMut name typeAnnot expr -> do
        let Unqualified unqual = name
        expr' <- resolveExpr expr
        typeAnnot' <- resolveTypeAnnot info typeAnnot
        checkNameDuplicate info unqual
        fullName <- insertNameToSet unqual
        return (DVar info isMut fullName typeAnnot' expr')

resolveStmt :: UntypedStmt -> Resolve UntypedStmt
resolveStmt = \case
    SExpr expr -> SExpr <$> resolveExpr expr
    SRet expr -> SRet <$> resolveExpr expr
    SWhile info cond body -> SWhile info <$> resolveExpr cond <*> resolveExpr body

resolveExpr :: UntypedExpr -> Resolve UntypedExpr
resolveExpr = \case
    lit@ELit {} -> return lit

    EVar info _ name -> EVar info () <$> resolveName info name

    EAssign info _ lhs rhs -> do
        lhs' <- resolveExpr lhs
        rhs' <- resolveExpr rhs
        return (EAssign info () lhs' rhs')

    EBlock info _ decls expr -> do
        tscope <- tmpScope
        local (++ [tscope]) (do
            decls' <- traverse resolveDecl decls
            expr' <- resolveExpr expr
            return (EBlock info () decls' expr'))

    EIf info _ cond exprA exprB -> do
        cond' <- resolveExpr cond
        exprA' <- resolveExpr exprA
        exprB' <- resolveExpr exprB
        return (EIf info () cond' exprA' exprB')

    EMatch info _ expr branches -> do
        expr' <- resolveExpr expr
        branches' <- traverse (runResolveBranch info) branches
        return (EMatch info () expr' branches')

    EBinOp info _ name lhs rhs -> do
        lhs' <- resolveExpr lhs
        rhs' <- resolveExpr rhs
        name' <- resolveName info name
        return (EBinOp info () name' lhs' rhs')

    EUnaOp info _ name expr -> do
        expr' <- resolveExpr expr
        name' <- resolveName info name
        return (EUnaOp info () name' expr')

    EClosure info _ cvars params tann expr -> do
        undefined

    ECall info _ expr args -> do
        expr' <- resolveExpr expr
        args' <- traverse resolveExpr args
        return (ECall info () expr' args')

    ECast info typ expr -> do
        typ' <- resolveType info typ
        expr' <- resolveExpr expr
        return (ECast info typ' expr')

    ERecordEmpty info _ -> return (ERecordEmpty info ())

    ERecordSelect info _ expr label -> do
        expr' <- resolveExpr expr
        return (ERecordSelect info () expr' label)

    ERecordRestrict info _ expr label -> do
        expr' <- resolveExpr expr
        return (ERecordRestrict info () expr' label)

    ERecordExtend info _ expr1 label expr2 -> do
        expr1' <- resolveExpr expr1
        expr2' <- resolveExpr expr2
        return (ERecordExtend info () expr1' label expr2')

    EVariant info _ enumName variantLabel exprs -> do
        enumName' <- resolveName info enumName
        exprs' <- traverse resolveExpr exprs
        verifyEnumAndVariantExists info enumName' variantLabel
        return (EVariant info () enumName' variantLabel exprs')

runResolveBranch info branch = do
    tscope <- tmpScope
    local (++ [tscope]) (resolveBranch info branch)

resolveBranch :: SyntaxInfo -> (Pattern, UntypedExpr) -> Resolve (Pattern, UntypedExpr)
resolveBranch _ (PWild, expr) = (PWild, ) <$> resolveExpr expr
resolveBranch _ (PVar (Unqualified name), expr) = do
    name' <- insertNameToSet name
    (PVar name', ) <$> resolveExpr expr
resolveBranch info (PVariant enumName label varNames, expr) = do
    enumName' <- resolveName info enumName

    verifyEnumAndVariantExists info enumName' label

    let varNamesUnquals = map (\(Unqualified unqual) -> unqual) varNames
    varNames' <- traverse insertNameToSet varNamesUnquals

    (PVariant enumName' label varNames', ) <$> resolveExpr expr
resolveBranch _ (PLit lit, expr) = do
    (PLit lit, ) <$> resolveExpr expr
resolveBranch _ _ = undefined

resolveType :: SyntaxInfo -> Type -> Resolve Type
resolveType info = \case
    typ@(TConst name@(Unqualified unqual)) | unqual `elem` baseTypes -> return typ

    typ@(TConst name) -> do
        name' <- resolveName info name
        aliases <- gets typeAliasMap
        verifyNameExists info name'
        -- Check if it's a type alias first
        case M.lookup name' aliases of
            Just parent -> resolveType info parent
            Nothing -> do
                enums <- gets enumMap
                if M.member name' enums
                    then return (TConst name')
                    -- This means a function or something else with the name of the type was being used as the type
                    else throwError (UndefinedError info (show name')) 

    TApp typ typs -> TApp <$> resolveType info typ <*> traverse (resolveType info) typs

    TArrow typs typ -> TArrow <$> traverse (resolveType info) typs <*> resolveType info typ

    TPtr typ -> TPtr <$> resolveType info typ

    tv@TVar {} -> return tv

    TRecordEmpty -> return TRecordEmpty

    TRecordExtend label fieldType rest -> TRecordExtend label <$> resolveType info fieldType <*> resolveType info rest
    where
        baseTypes =
            ["i8", "i16", "i32", "i64",
            "u8", "u16", "u32", "u64",
            "f32", "f64",
            "char", "str", "bool", "unit"]

resolveTypeAnnot :: SyntaxInfo -> TypeAnnot -> Resolve TypeAnnot
resolveTypeAnnot info annot =
    case annot of
        Just typ -> Just <$> resolveType info typ
        Nothing -> return Nothing

checkNameDuplicate :: SyntaxInfo -> Text -> Resolve ()
checkNameDuplicate info name = do
    curLocalScope <- ask
    set <- gets nameSet
    fullScope <- prependModulePath curLocalScope
    when (Qualified fullScope name `S.member` set)
        $ throwError (RedefinitionError info (show name))

insertNameToSet :: Text -> Resolve Name
insertNameToSet name = do
    curLocalScope <- ask
    fullScope <- prependModulePath curLocalScope
    set <- gets nameSet
    state <- get
    let fullName = Qualified fullScope name
    put (state { nameSet = S.insert fullName set })
    return fullName

resolveName :: SyntaxInfo -> Name -> Resolve Name
resolveName info name =
    case name of
        Unqualified unqual -> qualifyName info unqual
        Qualified {} -> name <$ verifyNameExists info name

verifyNameExists :: SyntaxInfo -> Name -> Resolve ()
verifyNameExists info (Unqualified unqual) =
    throwError (GenericAnalyzerError info "Attempt to verify unqualified name")
verifyNameExists info name@Qualified {} = do
    set <- gets nameSet
    unless (name `S.member` set) $ throwError (UndefinedError info (show name))

verifyEnumAndVariantExists :: SyntaxInfo -> Name -> Text -> Resolve ()
verifyEnumAndVariantExists info enumName variantLabel = do
    enums <- gets enumMap
    case M.lookup enumName enums of
        Nothing ->
            throwError (GenericAnalyzerError info ("Enum " ++ show enumName ++ " doesn't exist"))
        Just variants ->
            unless (variantLabel `elem` variants) $ throwError (GenericAnalyzerError info ("Variant '" ++ unpack variantLabel ++ "' of " ++ show enumName ++ " doesn't exist"))

qualifyName :: SyntaxInfo -> Text -> Resolve Name
qualifyName info name = do
    curLocalScope <- ask
    qualifyNameHelper curLocalScope >>= \case
        Just foundName -> return foundName
        Nothing -> do
            mod <- gets (fromJust . curMod) -- curMod should be a Just by this point
            set <- gets nameSet
            -- check imported modules
            let imports = modImports mod
            allImports <- (imports ++) . concat <$> traverse gatherAllPubImports imports
            let exists = concatMap (\imp -> let fullName = Qualified (snd imp) name in [fullName | fullName `S.member` set]) allImports
            isPubs <- gets pubMap
            case filter (\n -> fromMaybe False (M.lookup n isPubs)) exists of
                [] -> do
                    -- check module externs
                    externs <- gets externSet
                    if name `S.member` externs
                        then return (Unqualified name)
                        else throwError (UndefinedError info (show name))
                [onlyOne] -> return onlyOne
                multiple -> throwError (GenericAnalyzerError info ("Multiple definitions found: " ++ show multiple))
    where
        qualifyNameHelper localScope = do
            set <- gets nameSet
            fullScope <- prependModulePath localScope
            if Qualified fullScope name `S.member` set
                then (return . Just) (Qualified fullScope name)
                else case localScope of
                    [] -> return Nothing
                    _ -> qualifyNameHelper (init localScope)

gatherAllPubImports :: Import -> Resolve [Import]
gatherAllPubImports imp = do
    impMap <- gets importsMap
    let parentImps = fromMaybe [] (M.lookup (snd imp) impMap)
    let pubParentImps = filter fst parentImps
    toAppend <- traverse gatherAllPubImports pubParentImps
    return (pubParentImps ++ concat toAppend)

prependModulePath :: [Text] -> Resolve [Text]
prependModulePath scope = do
    mod <- gets (fromJust . curMod)
    return (modPath mod ++ modName mod : scope)

tmpScope :: Resolve Text
tmpScope = do
    state <- get
    put (state { tmpScopeCount = tmpScopeCount state + 1 })
    return . pack . ('_':) $ ([1..] >>= flip replicateM ['a'..'z']) !! tmpScopeCount state
