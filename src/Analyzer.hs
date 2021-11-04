{-# Language OverloadedStrings #-}
{-# Language LambdaCase #-}
{-# Language TupleSections #-}
{-# Language FlexibleContexts #-}

module Analyzer (analyze) where

import Data.Text (Text, pack, unpack)
import Data.Maybe
import Data.Functor.Identity
import Data.Foldable (traverse_)
import Data.Bifunctor
import qualified Data.Map as M
import qualified Data.Set as S
import Control.Monad.RWS
import Control.Monad.Except

import Text.Megaparsec.Pos

import Debug.Trace

import Syntax
import Type
import Substitution
import Name

import Resolver

type Env = M.Map Name (TypeScheme, Bool)

data AnalyzerError = AnalyzerError SourcePos String
type Infer = RWST [Import] [Constraint] InferState (Except AnalyzerError)

instance Show AnalyzerError where   
    show (AnalyzerError pos msg) = sourcePosPretty pos ++ "\n\t" ++ msg

data InferState = InferState
    { environment :: Env
    , structMap :: M.Map Name [(Text, Type)]
    , freshCount :: Int
    , topLvlTmps :: M.Map Name Type
    , mainExists :: Bool
    , modulePubs :: M.Map [Text] [Text]
    , pubsEnv :: M.Map Name Type
    } deriving (Show)

analyze :: UntypedProgram -> Either String TypedProgram 
analyze prog = case (runInfer . resolve) prog of
    Left err -> Left (show err)
    Right res -> Right res

-- Type Inference
constrain :: Constraint -> Infer ()
constrain = tell . (: [])

fresh :: Infer Type
fresh = do
    state <- get
    let count = freshCount state
    put (state { freshCount = count + 1 })
    (return . TVar . TV . pack) (names !! count)
    where
        names = map ('_' :) ([1..] >>= flip replicateM ['a'..'z'])

generalize :: Env -> Type -> TypeScheme
generalize env t = Forall (S.toList vs) t
    where vs = tvs t `S.difference` tvs (map fst $ M.elems env)

instantiate :: TypeScheme -> Infer Type
instantiate (Forall vs t) = do
    nvs <- traverse (const fresh) vs
    let sub = M.fromList (zip vs nvs)
    return (apply sub t)

runInfer :: UntypedProgram -> Either AnalyzerError TypedProgram
runInfer prog =
    let defaultState = InferState
            { environment = M.empty
            , structMap = M.empty
            , freshCount = 0
            , topLvlTmps = M.empty
            , mainExists = False
            , modulePubs = M.empty
            , pubsEnv = M.empty
            } in
    case runIdentity $ runExceptT $ runRWST (inferProgram prog) [] defaultState of
        Left err -> Left err
        Right (prog', _, consts) -> do
            sub <- runSolve consts
            return $ fmap (fmap $ apply sub) prog'

inferProgram :: UntypedProgram -> Infer TypedProgram
inferProgram mods = do
    traverse_ inferModulePre mods
    mods' <- traverse inferModule mods
    mainExistsCurr <- gets mainExists
    if mainExistsCurr
        then return mods'
        else err (SourcePos "" (mkPos 0) (mkPos 0)) "No 'main' function found in a top module called 'main'"

inferModulePre :: UntypedModule -> Infer ()
inferModulePre (Module pos name _ topLvls pubs) = do
    modPubs <- gets modulePubs

    case M.lookup name modPubs of
        Nothing -> do
            toInsert' <- concat <$> traverse generatePubVar topLvls
            let toInsert = M.fromList $ filter (\(n, _) -> extractName n `elem` pubs) toInsert'
            state <- get
            put (state { pubsEnv = pubsEnv state `M.union` toInsert, modulePubs = M.insert name pubs (modulePubs state) })
        _ -> err pos ("Already defined module " ++ show name)

generatePubVar :: UntypedTopLvl -> Infer [(Name, Type)]
generatePubVar = \case
    TLFunc _ _ name _ _ _ -> do
        var <- fresh
        return [(name, var)]
    TLOper _ _ oper _ _ _ -> do
        var <- fresh
        return [(oper, var)]
    TLExtern {} -> return []
    TLType _ _ _ cons -> do
        let (conNames, _) = unzip cons
        vars <- traverse (const fresh) conNames
        return (zip conNames vars)
    TLStruct _ name _ fields -> do
        state <- get
        put (state { structMap = M.insert name fields (structMap state) }) -- also add entry to struct-labels map

        var <- fresh
        return [(name, var)]

inferModule :: UntypedModule -> Infer TypedModule
inferModule (Module pos name imports topLvls pubs) = do
    traverse_ insertTmpVars topLvls
    topLvls' <- local (const imports) (traverse inferTopLvl topLvls)

    state <- get
    put (state { environment = M.empty
                , topLvlTmps = M.empty })
    return (Module pos name imports topLvls' pubs)

insertTmpVars :: UntypedTopLvl -> Infer ()
insertTmpVars = \case
    TLFunc _ _ name _ _ _ -> do
        var <- fresh
        state <- get
        put (state { topLvlTmps = M.insert name var (topLvlTmps state) })

    TLOper _ _ oper _ _ _ -> do
        var <- fresh
        state <- get
        put (state { topLvlTmps = M.insert oper var (topLvlTmps state) })

    TLType pos typeName typeParams cons -> insertValueCons pos typeName typeParams cons

    TLStruct pos name typeParams fields -> do
        let (labels, types) = unzip fields
        if any (checkInfiniteType name) types
            then err pos $ "Infinite type " ++ show name
            else do
                let varsTypeParams = S.fromList typeParams
                let varsTypes = tvs types
                let typeParams' = map TVar typeParams

                if (varsTypeParams `S.intersection` varsTypes) /= varsTypes
                    then let undefineds = S.toList (varsTypes `S.difference` varsTypeParams)
                         in err pos ("Undefined type variables " ++ show undefineds)
                    else let typ = TFunc types (TCon name typeParams')
                             scheme = Forall [] typ
                         in do
                            pubsEnv <- gets pubsEnv
                            case M.lookup name pubsEnv of
                                Just t -> constrain (CEqual pos typ t)
                                Nothing -> return ()
                            insertEnv (name, (scheme, False))

    TLExtern name ptypes rtype -> insertEnv (Unqualified name, (Forall [] $ TFunc ptypes rtype, False))

insertValueCons :: SourcePos -> Name -> [TVar] -> [(Name, [Type])] -> Infer ()
insertValueCons _ _ _ [] = return ()
insertValueCons pos typeName typeParams ((conName, conTypes) : restCons) = do
    if any (checkInfiniteType typeName) conTypes
        then err pos $ "Infinite type " ++ show typeName ++ " (con. " ++ show conName ++ ")"
        else do
            let varsTypeParams = S.fromList typeParams
            let varsCon = tvs conTypes
            let typeParams' = map TVar typeParams

            -- env <- gets environment
            if (varsTypeParams `S.intersection` varsCon) /= varsCon
                then let undefineds = S.toList (varsCon `S.difference` varsTypeParams)
                     in err pos ("Undefined type variables " ++ show undefineds)
                else let typ = case conTypes of
                            [] -> TCon typeName typeParams' -- generalize env (TParam typeNam)
                            _ -> TFunc conTypes (TCon typeName typeParams') --generalize env (TFunc (TParam ttypeParams' (TCon typeName))
                         scheme = Forall [] typ
                     in do
                        pubsEnv <- gets pubsEnv
                        case M.lookup conName pubsEnv of
                            Just t -> constrain (CEqual pos typ t)
                            Nothing -> return ()
                        insertEnv (conName, (scheme, False)) *> insertValueCons pos typeName typeParams restCons

checkInfiniteType :: Name -> Type -> Bool
checkInfiniteType typeName (TCon name _) = name == typeName
checkInfiniteType typeName (TArray t) = checkInfiniteType typeName t
checkInfiniteType _ _ = False

inferTopLvl :: UntypedTopLvl -> Infer TypedTopLvl
inferTopLvl = \case
    TLFunc _ pos name params rtann body -> do
        alreadyDefined <- exists name
        if alreadyDefined then err pos ("Function '" ++ show name ++ "' already defined")
        else do
            case name of
                Qualified name -> 
                    when (name == ["main", "main"]) (do
                        state <- get
                        put (state { mainExists = True }))
                _ -> return ()
            (body', typ) <- inferFn pos name params rtann body
            return (TLFunc typ pos name params rtann body')

    TLOper _ pos oper params rtann body -> do
        alreadyDefined <- exists oper
        if alreadyDefined then err pos ("Operator '" ++ show oper ++ "' already defined")
        else do
            (body', typ) <- inferFn pos oper params rtann body
            return (TLOper typ pos oper params rtann body')

    TLType pos typeName typeParams cons -> return (TLType pos typeName typeParams cons)
    TLStruct pos name typeParams fields -> return (TLStruct pos name typeParams fields)
    TLExtern name ptypes rtype -> return (TLExtern name ptypes rtype)

inferFn :: SourcePos -> Name -> Params -> TypeAnnot -> UntypedExpr -> Infer (TypedExpr, Type)
inferFn pos name params rtann body = do
    let (pnames, panns) = unzip params
    ptypes <- traverse (const fresh) params
    let ptypesSchemes = map ((, False) . Forall []) ptypes
    let nenv = M.fromList (zip (map Unqualified pnames) ptypesSchemes)
    ((body', rtype), consts) <- listen (scoped (`M.union` nenv) (inferExpr body))

    subst <- liftEither (runSolve consts)
    env <- gets environment
    let typ = apply subst (TFunc ptypes rtype)
        scheme = generalize env typ

    let (TFunc ptypes' rtype') = typ
    when (isJust rtann) (constrain $ CEqual pos rtype' (fromJust rtann))
    sequence_ [when (isJust pann) (constrain $ CEqual pos ptype (fromJust pann)) | (ptype, pann) <- zip ptypes' panns]

    -- traverse_ (constrain . CEqual pos rtype') (searchReturnsExpr body')

    state <- get
    let tmpsEnv = topLvlTmps state
    constrain (CEqual pos typ (fromJust (M.lookup name tmpsEnv)))
    put (state {topLvlTmps = M.delete name tmpsEnv})

    pubsEnv <- gets pubsEnv
    let lookup = M.lookup name pubsEnv
    when (isJust lookup) (constrain (CEqual pos typ (fromJust lookup)))
    
    insertEnv (name, (scheme, False)) -- already scoped by block
    return (body', typ)

inferDecl :: UntypedDecl -> Infer TypedDecl
inferDecl = \case
    DVar _ pos isMut name tann expr -> do
        alreadyDefined <- exists (Unqualified name)
        if alreadyDefined then err pos ("Variable '" ++ unpack name ++ "' already defined")
        else do
            ((expr', etype), consts) <- listen (inferExpr expr)
            subst <- liftEither (runSolve consts)
            env <- ask
            let typ = apply subst etype
                scheme = Forall [] typ -- TODO: generalize
            when (isJust tann) (constrain $ CEqual pos typ (fromJust tann))
            insertEnv (Unqualified name, (scheme, isMut))
            return (DVar typ pos isMut name tann expr')
    DStmt s -> DStmt <$> inferStmt s

inferStmt :: UntypedStmt -> Infer TypedStmt
inferStmt = \case
    SRet expr -> do
        (expr', _) <- inferExpr expr
        return (SRet expr')
    SWhile pos cond body -> do
        (cond', ctype) <- inferExpr cond
        (body', _) <- inferExpr body
        constrain (CEqual pos ctype TBool)
        return (SWhile pos cond' body')
    SExpr expr -> do
        (expr', _) <- inferExpr expr
        return (SExpr expr')

inferExpr :: UntypedExpr -> Infer (TypedExpr, Type)
inferExpr = \case
    ELit _ pos lit -> let typ = inferLit lit in return (ELit typ pos lit, typ)

    EVar _ pos _ name -> do
        (typ, newName) <- lookupType pos name
        let tvars = S.toList (tvs typ)
        return (EVar typ pos (map TVar tvars) newName, typ)

    EAssign _ pos l r -> do
        (l', ltype) <- inferExpr l
        (r', rtype) <- inferExpr r
        constrain (CEqual pos ltype rtype)
        let expr' = EAssign ltype pos l' r'
        case l' of
            EVar _ vpos _ name -> do
                isMut <- lookupMut vpos name
                unless isMut (err pos $ "Cannot assign to immutable variable " ++ show name)
                return (expr', ltype)
            EIndex _ _ (EVar _ vpos _ name) _ -> do
                isMut <- lookupMut vpos name
                unless isMut (err pos $ "Cannot assign to immutable variable " ++ show name)
                return (expr', ltype)
            EAccess _ _ (EVar _ vpos _ name) _ -> do
                isMut <- lookupMut vpos name
                unless isMut (err pos $ "Cannot assign to immutable variable " ++ show name)
                return (expr', ltype)
            EDeref {} -> return (expr', ltype)
            _ -> err pos "Cannot assign to non-lvalue"

    EBlock _ pos origDecls expr -> do
        (decls', expr', etype) <- scoped id (do
            ds <- traverse inferDecl origDecls
            (ex, et) <- inferExpr expr
            return (ds, ex, et))
        return (EBlock etype pos decls' expr', etype)

    EIf _ pos cond texpr fexpr -> do
        (cond', ctype) <- inferExpr cond
        (texpr', ttype) <- inferExpr texpr
        (fexpr', ftype) <- inferExpr fexpr
        constrain (CEqual pos ctype TBool)
        constrain (CEqual pos ttype ftype)
        return (EIf ttype pos cond' texpr' fexpr', ttype)

    EMatch _ pos mexpr branches -> do
        (mexpr', mtype) <- inferExpr mexpr
        (branches', btypes) <- unzip <$> traverse (inferBranch pos mtype) branches
        case btypes of
            [] -> err pos "Empty match expression"
            (btype : rest) -> (EMatch btype pos mexpr' branches', btype) <$ traverse_ (constrain . CEqual pos btype) rest

    EBinOp _ pos oper a b -> do
        (a', at) <- inferExpr a
        (b', bt) <- inferExpr b
        case oper of
            _ | extractName oper `elem` ["+", "-", "*", "/"] -> do -- TODO
                return (EBinOp at pos oper a' b', at)
            _ | extractName oper `elem` ["==", "!=", ">", "<", ">=", "<="] -> do
                return (EBinOp TBool pos oper a' b', TBool)
            _ | extractName oper `elem` ["||", "&&"] -> do
                constrain (CEqual pos at TBool)
                constrain (CEqual pos bt TBool)
                return (EBinOp TBool pos oper a' b', TBool)
            _ -> do
                (opt, newName) <- lookupType pos oper
                rt <- fresh
                let ft = TFunc [at, bt] rt
                constrain (CEqual pos opt ft)
                return (EBinOp rt pos newName a' b', rt)

    EUnaOp _ pos oper expr -> do
        (opt, newName) <- lookupType pos oper
        (a', at) <- inferExpr expr
        rt <- fresh
        constrain (CEqual pos opt (TFunc [at] rt))
        return (EUnaOp rt pos oper a', rt)

    EClosure _ pos closedVars params rtann body -> err pos "Closures not implemented yet"

    ECall _ pos expr args -> do
        (a', at) <- inferExpr expr
        (bs', bts) <- unzip <$> traverse inferExpr args
        rt <- fresh
        constrain (CEqual pos at (TFunc bts rt))
        return (ECall rt pos a' bs', rt)

    ECast _ pos targ expr -> do
        (expr', etype) <- inferExpr expr
        return (ECast targ pos targ expr', targ) -- TODO

    EDeref _ pos expr -> do
        (expr', etype) <- inferExpr expr
        tv <- fresh
        constrain (CEqual pos etype (TPtr tv))
        return (EDeref tv pos expr', tv)

    ERef _ pos expr -> do
        (expr', etype) <- inferExpr expr
        case expr' of
            EVar _ _ _ s -> return (ERef (TPtr etype) pos expr', TPtr etype)
            _ -> err pos "Cannot reference non-variable"

    ESizeof _ pos arg -> do
        arg' <- case arg of
            Left t -> return (Left t)
            Right e -> Right . fst <$> inferExpr e
        return (ESizeof TInt32 pos arg', TInt32)

    EArray _ pos exprs -> do
        (exprs', ets) <- unzip <$> traverse inferExpr exprs
        tv <- fresh
        sequence_ [constrain (CEqual pos tv et) | et <- ets]
        let typ = TArray tv
        return (EArray typ pos exprs', typ)

    EIndex _ pos expr idx -> do
        (expr', etype) <- inferExpr expr
        tv <- fresh
        let typ = TArray tv
        constrain (CEqual pos typ etype)
        return (EIndex tv pos expr' idx, tv)

    EStruct _ pos structName fields -> do
        let (labels, exprs) = unzip fields
        (exprs', ets) <- unzip <$> traverse inferExpr exprs
        (typ, newName) <- lookupType pos structName
        rt <- fresh
        constrain (CEqual pos typ (TFunc ets rt))

        structMap <- gets structMap
        let (labels', _) = unzip $ fromJust (M.lookup newName structMap) -- struct labels should be defined if its type was
        
        if labels /= labels'
            then err pos ("Invalid/missing field names when instancing struct " ++ show structName)
            else return (EStruct rt pos newName (zip labels exprs'), rt)

    EAccess _ pos expr label -> do
        ((expr', et), consts) <- listen (inferExpr expr)
        subst <- liftEither (runSolve consts)
        let typ = apply subst et

        case typ of
            TCon name tparams -> do
                structMap <- gets structMap
                let entry = M.lookup name structMap
                unless (isJust entry) (err pos (show name ++ " is not a struct"))
                case lookup label (fromJust entry) of
                    Just t -> return (EAccess t pos expr' label, t)
                    Nothing -> err pos ("Struct " ++ show name ++ " has no field '" ++ show label ++ "'")
            _ -> err pos ("Unable to infer type of expression; please provide type annotations (accessing " ++ show label ++ ")")

inferLit :: Lit -> Type
inferLit = \case
    LInt _ -> TInt32
    LFloat _ -> TFloat64
    LString _ -> TStr
    LChar _ -> TChar
    LBool _ -> TBool
    LUnit -> TUnit

inferBranch :: SourcePos -> Type -> (Pattern, UntypedExpr) -> Infer ((Pattern, TypedExpr), Type)
inferBranch pos mt (pat, expr) = do
    (pt, vars) <- inferPattern pos pat
    let vars' = map (\(s, ts) -> (s, (ts, False))) vars
    constrain (CEqual pos pt mt)
    (expr', et) <- scoped (M.fromList vars' `M.union`) (inferExpr expr)
    return ((pat, expr'), et)

inferPattern :: SourcePos -> Pattern -> Infer (Type, [(Name, TypeScheme)])
inferPattern pos (PVar name) = do
    ptype <- fresh
    pure (ptype, [(Unqualified name, Forall [] ptype)])
inferPattern _ (PLit lit) = return (inferLit lit, [])
inferPattern _ PWild = do
    ptype <- fresh
    return (ptype, [])
inferPattern pos (PCon conName binds) = do
    ptypes <- traverse (const fresh) binds
    let res = [(Unqualified bind, Forall [] ptype) | (bind, ptype) <- zip binds ptypes]
    (conType, _) <- lookupType pos conName
    t <- fresh
    let conType' = case binds of
            [] -> t
            _ -> TFunc ptypes t
    constrain (CEqual pos conType' conType)
    return (t, res)

-- Environment helpers
scoped :: (Env -> Env) -> Infer a -> Infer a
scoped fn m = do
    state <- get
    let env = environment state
    put (state { environment = fn env })
    res <- m
    state' <- get
    put (state' { environment = env })
    return res

insertEnv :: (Name, (TypeScheme, Bool)) -> Infer ()
insertEnv (name, info) = do
    state <- get
    put (state { environment = M.insert name info (environment state) })

lookupVar :: SourcePos -> Name -> Infer ((Type, Name), Bool)
lookupVar pos name = do
    env <- gets environment
    case M.lookup name env of
        Just (ts, m) -> (\t -> ((t, name), m)) <$> instantiate ts
        Nothing -> case M.lookup (Unqualified (extractName name)) env of 
            Just (ts, m) ->  (\t -> ((t, Unqualified (extractName name)), m)) <$> instantiate ts
            Nothing -> do -- check temp env for top levels
                tmpsEnv <- gets topLvlTmps
                case M.lookup name tmpsEnv of
                    Just v -> return ((v, name), False)
                    Nothing -> ask >>= checkImports pos name

checkImports :: SourcePos -> Name -> [Import] -> Infer ((Type, Name), Bool)
checkImports pos name [] = err pos ("Unknown identifier " ++ show name)
checkImports pos name (imp : imps) = do
    moduleMap <- gets modulePubs
    pubsTypes <- gets pubsEnv
    let pubs = fromMaybe [] (M.lookup imp moduleMap)
    if extractName name `elem` pubs
        then let newName = Qualified (imp ++ [extractName name]) in return ((fromJust (M.lookup newName pubsTypes), newName), False)
        else checkImports pos name imps

lookupType :: SourcePos -> Name -> Infer (Type, Name)
lookupType pos name = fst <$> lookupVar pos name

lookupMut :: SourcePos -> Name -> Infer Bool
lookupMut pos name = snd <$> lookupVar pos name

exists :: Name -> Infer Bool
exists name = isJust . M.lookup name <$> gets environment -- Doesn't check temp env for top levels

-- Unification
type Solve = ExceptT AnalyzerError Identity

compose :: Substitution -> Substitution -> Substitution
compose a b = M.map (apply a) b `M.union` a

unify :: SourcePos -> Type -> Type -> Solve Substitution
unify _ a b | a == b = return M.empty
unify pos (TVar v) t = bind pos v t
unify pos t (TVar v) = bind pos v t
unify pos a@(TCon c1 ts1) b@(TCon c2 ts2)
    | c1 /= c2 = err pos $ "Type mismatch " ++ show a ++ " ~ " ++ show b
    | otherwise = unifyMany pos ts1 ts2

unifyMany :: SourcePos -> [Type] -> [Type] -> Solve Substitution
unifyMany _ [] [] = return M.empty
unifyMany pos (t1 : ts1) (t2 : ts2) =
  do su1 <- unify pos t1 t2
     su2 <- unifyMany pos (apply su1 ts1) (apply su1 ts2)
     return (su2 `compose` su1)
unifyMany pos t1 t2 = err pos $ "Type mismatch " ++ show (head t1) ++ " ~ " ++ show (head t2)

bind :: SourcePos -> TVar -> Type -> Solve Substitution
bind pos v t
    | v `S.member` tvs t = err pos $ "Infinite type " ++ show v ++ " ~ " ++ show t
    | otherwise = return $ M.singleton v t 

solve :: Substitution -> [Constraint] -> Solve Substitution
solve s c =
    case c of
        [] -> return s
        (CEqual pos t1 t2 : cs) -> do
            s1 <- unify pos t1 t2
            let nsub = s1 `compose` s
            solve (s1 `compose` s) (apply s1 cs)

runSolve :: [Constraint] -> Either AnalyzerError Substitution
runSolve cs = runIdentity $ runExceptT $ solve M.empty cs

-- Utility
err :: (MonadError AnalyzerError m) => SourcePos -> String -> m a
err pos msg = throwError (AnalyzerError pos msg)
