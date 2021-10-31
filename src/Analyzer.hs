{-# Language OverloadedStrings #-}
{-# Language LambdaCase #-}
{-# Language TupleSections #-}

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

import Debug.Trace

import Syntax
import Type
import Substitution
import Name

import Resolver

type AEnv = M.Map Name (TypeScheme, Bool)
type Infer = RWST [Import] [Constraint] InferState (Except String)

data InferState = InferState
    { environment :: AEnv
    , freshCount :: Int
    , topLvlTmps :: M.Map Name Type
    , mainExists :: Bool
    , modulePubs :: M.Map [Text] [Text]
    , pubsEnv :: M.Map Name Type
    } deriving (Show)

analyze :: UntypedProgram -> Either String TypedProgram 
analyze = runInfer . resolve

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

generalize :: AEnv -> Type -> TypeScheme
generalize env t = Forall (S.toList vs) t
    where vs = tvs t `S.difference` tvs (map fst $ M.elems env)

instantiate :: TypeScheme -> Infer Type
instantiate (Forall vs t) = do
    nvs <- traverse (const fresh) vs
    let sub = M.fromList (zip vs nvs)
    return (apply sub t)

runInfer :: UntypedProgram -> Either String TypedProgram
runInfer prog =
    let defaultState = InferState
            { environment = M.empty
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
        else throwError "No 'main' function found in a top module called 'main'"

inferModulePre :: UntypedModule -> Infer ()
inferModulePre (Module name _ topLvls pubs) = do
    modPubs <- gets modulePubs

    case M.lookup name modPubs of
        Nothing -> do
            toInsert' <- concat <$> traverse generatePubVar topLvls
            let toInsert = M.fromList $ filter (\(n, _) -> extractName n `elem` pubs) toInsert'
            state <- get
            put (state { pubsEnv = pubsEnv state `M.union` toInsert, modulePubs = M.insert name pubs (modulePubs state) })
        _ -> throwError ("Already defined module " ++ show name)

generatePubVar :: UntypedTopLvl -> Infer [(Name, Type)]
generatePubVar = \case
    TLFunc _ name _ _ _ -> do
        var <- fresh
        return [(name, var)]
    TLOper _ _ oper _ _ _ -> do
        var <- fresh
        return [(oper, var)]
    TLExtern {} -> return []
    TLType _ _ cons -> do
        let (conNames, _) = unzip cons
        vars <- traverse (const fresh) conNames
        return (zip conNames vars)

inferModule :: UntypedModule -> Infer TypedModule
inferModule (Module name imports topLvls pubs) = do
    traverse_ insertTmpVars topLvls
    topLvls' <- local (const imports) (traverse inferTopLvl topLvls)

    state <- get
    put (state { environment = M.empty
                , topLvlTmps = M.empty })
    return (Module name imports topLvls' pubs)

insertTmpVars :: UntypedTopLvl -> Infer ()
insertTmpVars = \case
    TLFunc _ name _ _ _ -> do
        var <- fresh
        state <- get
        put (state { topLvlTmps = M.insert name var (topLvlTmps state) })

    TLOper _ _ oper _ _ _ -> do
        var <- fresh
        state <- get
        put (state { topLvlTmps = M.insert oper var (topLvlTmps state) })

    TLType typeName typeParams cons -> insertValueCons typeName typeParams cons

    TLExtern name ptypes rtype -> insertEnv (Unqualified name, (Forall [] $ TFunc ptypes rtype, False))

insertValueCons :: Name -> [TVar] -> [(Name, [Type])] -> Infer ()
insertValueCons _ _ [] = return ()
insertValueCons typeName typeParams ((conName, conTypes) : restCons) = do
    if any (checkInfiniteType typeName) conTypes
        then throwError $ "Infinite type " ++ show typeName ++ " (con. " ++ show conName ++ ")"
        else do
            let typeParams' = map TVar typeParams
            let varsTypeParams = tvs typeParams'
            let varsCon = tvs conTypes

            env <- gets environment
            if (varsTypeParams `S.intersection` varsCon) /= varsCon
                then let undefineds = S.toList (varsCon `S.difference` varsTypeParams)
                     in throwError ("Undefined type variables " ++ show undefineds)
                else let typ = case conTypes of
                            [] -> TCon typeName typeParams' -- generalize env (TParam typeNam)
                            _ -> TFunc conTypes (TCon typeName typeParams') --generalize env (TFunc (TParam ttypeParams' (TCon typeName))
                         scheme = Forall [] typ
                     in insertEnv (conName, (scheme, False)) *> insertValueCons typeName typeParams restCons
    where
        checkInfiniteType typeName (TCon name _) = name == typeName
        checkInfiniteType typeName (TArray t) = checkInfiniteType typeName t
        checkInfiniteType _ _ = False

inferTopLvl :: UntypedTopLvl -> Infer TypedTopLvl
inferTopLvl = \case
    TLFunc _ name params rtann body -> do
        alreadyDefined <- exists name
        if alreadyDefined then throwError ("Function '" ++ show name ++ "' already defined")
        else do
            case name of
                Qualified name -> 
                    when (name == ["main", "main"]) (do
                        state <- get
                        put (state { mainExists = True }))
                _ -> return ()
            (body', typ) <- inferFn name params rtann body
            return (TLFunc typ name params rtann body')

    TLOper _ opdef oper params rtann body -> do
        alreadyDefined <- exists oper
        if alreadyDefined then throwError ("Operator '" ++ show oper ++ "' already defined")
        else do
            (body', typ) <- inferFn oper params rtann body
            return (TLOper typ opdef oper params rtann body')

    TLType typeName typeParams cons -> return (TLType typeName typeParams cons)
    TLExtern name ptypes rtype -> return (TLExtern name ptypes rtype)

inferFn :: Name -> Params -> TypeAnnot -> UntypedExpr -> Infer (TypedExpr, Type)
inferFn name params rtann body = do
    let (pnames, panns) = unzip params
    ptypes <- traverse (const fresh) params
    let ptypesSchemes = map ((, False) . Forall []) ptypes
    let nenv = M.fromList (zip (map Unqualified pnames) ptypesSchemes)
    ((body', rtype), consts) <- listen (scoped (`M.union` nenv) (inferExpr body))

    subst <- liftEither (runSolve consts)
    env <- gets environment
    let typ = apply subst (TFunc ptypes rtype)
        scheme = generalize env typ -- TODO: generalize for parametric polymorphism

    let (TFunc ptypes' rtype') = typ
    when (isJust rtann) (constrain $ CEqual rtype' (fromJust rtann))
    sequence_ [when (isJust pann) (constrain $ CEqual ptype (fromJust pann)) | (ptype, pann) <- zip ptypes' panns]

    retStmtTypes <- searchReturnsExpr body'
    traverse_ (constrain . CEqual rtype') retStmtTypes

    state <- get
    let tmpsEnv = topLvlTmps state
    constrain (CEqual typ (fromJust (M.lookup name tmpsEnv)))
    put (state {topLvlTmps = M.delete name tmpsEnv})

    pubsEnv <- gets pubsEnv
    let lookup = M.lookup name pubsEnv
    when (isJust lookup) (constrain (CEqual typ (fromJust lookup)))
    
    insertEnv (name, (scheme, False)) -- already scoped by block
    return (body', typ)

inferDecl :: UntypedDecl -> Infer TypedDecl
inferDecl = \case
    DVar _ isMut name tann expr -> do
        alreadyDefined <- exists (Unqualified name)
        if alreadyDefined then throwError ("Variable '" ++ unpack name ++ "' already defined")
        else do
            ((expr', etype), consts) <- listen (inferExpr expr)
            subst <- liftEither (runSolve consts)
            env <- ask
            let typ = apply subst etype
                scheme = Forall [] typ -- TODO: generalize
            when (isJust tann) (constrain $ CEqual typ (fromJust tann))
            insertEnv (Unqualified name, (scheme, isMut))
            return (DVar typ isMut name tann expr')
    DStmt s -> DStmt <$> inferStmt s

inferStmt :: UntypedStmt -> Infer TypedStmt
inferStmt = \case
    SRet expr -> do
        (expr', _) <- inferExpr expr
        return (SRet expr')
    SWhile cond body -> do
        (cond', ctype) <- inferExpr cond
        (body', _) <- inferExpr body
        constrain (CEqual ctype TBool)
        return (SWhile cond' body')
    SExpr expr -> do
        (expr', _) <- inferExpr expr
        return (SExpr expr')

inferExpr :: UntypedExpr -> Infer (TypedExpr, Type)
inferExpr = \case
    ELit _ lit -> let typ = inferLit lit in return (ELit typ lit, typ)

    EVar _ _ name -> do
        (typ, newName) <- lookupType name
        let tvars = S.toList (tvs typ)
        return (EVar typ (map TVar tvars) newName, typ)

    EAssign _ l r -> do
        (l', ltype) <- inferExpr l
        (r', rtype) <- inferExpr r
        constrain (CEqual ltype rtype)
        let expr' = EAssign ltype l' r'
        case l' of
            EVar _ _ name -> do
                isMut <- lookupMut name
                unless isMut (throwError $ "Cannot assign to immutable variable " ++ show name)
                return (expr', ltype)
            EIndex _ (EVar _ _ name) idx -> do
                isMut <- lookupMut name
                unless isMut (throwError $ "Cannot assign to immutable variable " ++ show name)
                return (expr', ltype)
            EDeref _ _ -> return (expr', ltype)
            _ -> throwError "Cannot assign to non-lvalue"

    EBlock _ origDecls expr -> do
        (decls', expr', etype) <- scoped id (do
            ds <- traverse inferDecl origDecls
            (ex, et) <- inferExpr expr
            return (ds, ex, et))
        return (EBlock etype decls' expr', etype)

    EIf _ cond texpr fexpr -> do
        (cond', ctype) <- inferExpr cond
        (texpr', ttype) <- inferExpr texpr
        (fexpr', ftype) <- inferExpr fexpr
        constrain (CEqual ctype TBool)
        constrain (CEqual ttype ftype)
        return (EIf ttype cond' texpr' fexpr', ttype)

    EMatch _ mexpr branches -> do
        (mexpr', mtype) <- inferExpr mexpr
        (branches', btypes) <- unzip <$> traverse (inferBranch mtype) branches
        case btypes of
            [] -> throwError "Empty match expression"
            (btype : rest) -> (EMatch btype mexpr' branches', btype) <$ traverse_ (constrain . CEqual btype) rest

    EBinOp _ oper a b -> do
        (a', at) <- inferExpr a
        (b', bt) <- inferExpr b
        case oper of
            _ | extractName oper `elem` ["+", "-", "*", "/"] -> do -- TODO
                return (EBinOp at oper a' b', at)
            _ | extractName oper `elem` ["==", "!=", ">", "<", ">=", "<="] -> do
                return (EBinOp TBool oper a' b', TBool)
            _ | extractName oper `elem` ["||", "&&"] -> do
                constrain (CEqual at TBool)
                constrain (CEqual bt TBool)
                return (EBinOp TBool oper a' b', TBool)
            _ -> do
                (opt, newName) <- lookupType oper
                rt <- fresh
                let ft = TFunc [at, bt] rt
                constrain (CEqual opt ft)
                return (EBinOp rt newName a' b', rt)

    EUnaOp _ oper expr -> do
        (opt, newName) <- lookupType oper
        (a', at) <- inferExpr expr
        rt <- fresh
        constrain (CEqual opt (TFunc [at] rt))
        return (EUnaOp rt oper a', rt)

    EClosure _ closedVars params rtann body -> throwError "Closures not implemented yet"

    ECall _ expr args -> do
        (a', at) <- inferExpr expr
        (bs', bts) <- unzip <$> traverse inferExpr args
        rt <- fresh
        constrain (CEqual at (TFunc bts rt))
        return (ECall rt a' bs', rt)

    ECast _ targ expr -> do
        (expr', etype) <- inferExpr expr
        return (ECast targ targ expr', targ) -- TODO

    EDeref _ expr -> do
        (expr', etype) <- inferExpr expr
        tv <- fresh
        constrain (CEqual etype (TPtr tv))
        return (EDeref tv expr', tv)

    ERef _ expr -> do
        (expr', etype) <- inferExpr expr
        case expr' of
            EVar _ _ s -> return (ERef (TPtr etype) expr', TPtr etype)
            _ -> throwError "Cannot reference non-variable"

    ESizeof _ arg -> do
        arg' <- case arg of
            Left t -> return (Left t)
            Right e -> Right . fst <$> inferExpr e
        return (ESizeof TInt32 arg', TInt32)

    EArray _ exprs -> do
        (exprs', ets) <- unzip <$> traverse inferExpr exprs
        tv <- fresh
        sequence_ [constrain (CEqual tv et) | et <- ets]
        let typ = TArray tv
        return (EArray typ exprs', typ)

    EIndex _ expr idx -> do
        (expr', etype) <- inferExpr expr
        tv <- fresh
        let typ = TArray tv
        constrain (CEqual typ etype)
        return (EIndex tv expr' idx, tv)

inferLit :: Lit -> Type
inferLit = \case
    LInt _ -> TInt32
    LFloat _ -> TFloat64
    LString _ -> TStr
    LChar _ -> TChar
    LBool _ -> TBool
    LUnit -> TUnit

inferBranch :: Type -> (Pattern, UntypedExpr) -> Infer ((Pattern, TypedExpr), Type)
inferBranch mt (pat, expr) = do
    (pt, vars) <- inferPattern pat
    let vars' = map (\(s, ts) -> (s, (ts, False))) vars
    constrain (CEqual pt mt)
    (expr', et) <- scoped (M.fromList vars' `M.union`) (inferExpr expr)
    return ((pat, expr'), et)

inferPattern :: Pattern -> Infer (Type, [(Name, TypeScheme)])
inferPattern (PVar name) = do
    ptype <- fresh
    pure (ptype, [(Unqualified name, Forall [] ptype)])
inferPattern (PLit lit) = return (inferLit lit, [])
inferPattern PWild = do
    ptype <- fresh
    return (ptype, [])
inferPattern (PCon conName binds) = do
    ptypes <- traverse (const fresh) binds
    let res = [(Unqualified bind, Forall [] ptype) | (bind, ptype) <- zip binds ptypes]
    (conType, _) <- lookupType conName
    t <- fresh
    let conType' = case binds of
            [] -> t
            _ -> TFunc ptypes t
    constrain (CEqual conType' conType)
    return (t, res)

-- Environment helpers
scoped :: (AEnv -> AEnv) -> Infer a -> Infer a
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

lookupVar :: Name -> Infer ((Type, Name), Bool)
lookupVar name = do
    env <- gets environment
    case M.lookup name env of
        Just (ts, m) -> (\t -> ((t, name), m)) <$> instantiate ts
        Nothing -> case M.lookup (Unqualified (extractName name)) env of 
            Just (ts, m) ->  (\t -> ((t, Unqualified (extractName name)), m)) <$> instantiate ts
            Nothing -> do -- check temp env for top levels
                tmpsEnv <- gets topLvlTmps
                case M.lookup name tmpsEnv of
                    Just v -> return ((v, name), False)
                    Nothing -> ask >>= checkImports name

checkImports :: Name -> [Import] -> Infer ((Type, Name), Bool)
checkImports name [] = throwError ("Unknown identifier " ++ show name)
checkImports name (imp : imps) = do
    moduleMap <- gets modulePubs
    pubsTypes <- gets pubsEnv
    let pubs = fromMaybe [] (M.lookup imp moduleMap)
    if extractName name `elem` pubs
        then let newName = Qualified (imp ++ [extractName name]) in return ((fromJust (M.lookup newName pubsTypes), newName), False)
        else checkImports name imps

lookupType :: Name -> Infer (Type, Name)
lookupType name = fst <$> lookupVar name

lookupMut :: Name -> Infer Bool
lookupMut name = snd <$> lookupVar name

exists :: Name -> Infer Bool
exists name = isJust . M.lookup name <$> gets environment -- Doesn't check temp env for top levels

-- Unification
type Solve = ExceptT String Identity

compose :: Substitution -> Substitution -> Substitution
compose a b = M.map (apply a) b `M.union` a

unify :: Type -> Type -> Solve Substitution
unify a b | a == b = return M.empty
unify (TVar v) t = bind v t
unify t (TVar v) = bind v t
unify a@(TCon c1 ts1) b@(TCon c2 ts2)
    | c1 /= c2 = throwError $ "Type mismatch " ++ show a ++ " ~ " ++ show b
    | otherwise = unifyMany ts1 ts2
unify a@(TFunc pts rt) b@(TFunc pts2 rt2)
    | length pts /= length pts2 = throwError $ "Type mismatch " ++ show a ++ " ~ " ++ show b
    | otherwise = unifyMany (rt : pts) (rt2 : pts2)
unify (TPtr t) (TPtr t2) = unify t t2
unify a@(TArray t) b@(TArray t2) = unify t t2
unify a b = throwError $ "Type mismatch " ++ show a ++ " ~ " ++ show b

unifyMany :: [Type] -> [Type] -> Solve Substitution
unifyMany [] [] = return M.empty
unifyMany (t1 : ts1) (t2 : ts2) =
  do su1 <- unify t1 t2
     su2 <- unifyMany (apply su1 ts1) (apply su1 ts2)
     return (su2 `compose` su1)
unifyMany t1 t2 = throwError $ "Type mismatch " ++ show (head t1) ++ " ~ " ++ show (head t2)

bind :: TVar -> Type -> Solve Substitution
bind v t
    | v `S.member` tvs t = throwError $ "Infinite type " ++ show v ++ " ~ " ++ show t
    | otherwise = return $ M.singleton v t 

solve :: Substitution -> [Constraint] -> Solve Substitution
solve s c =
    case c of
        [] -> return s
        (CEqual t1 t2 : cs) -> do
            s1 <- unify t1 t2
            let nsub = s1 `compose` s
            solve (s1 `compose` s) (apply s1 cs)

runSolve :: [Constraint] -> Either String Substitution
runSolve cs = runIdentity $ runExceptT $ solve M.empty cs

-- Utility
searchReturnsDecl :: TypedDecl -> Infer [Type]
searchReturnsDecl (DStmt (SRet expr)) = return [typeOfExpr expr]
searchReturnsDecl _ = return []

searchReturnsExpr :: TypedExpr -> Infer [Type]
searchReturnsExpr (EBlock _ decls _) = do
    res <- traverse searchReturnsDecl decls
    return (concat res)
searchReturnsExpr _ = return []
