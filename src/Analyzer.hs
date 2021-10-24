{-# Language TupleSections #-}
{-# Language LambdaCase #-}

module Analyzer (infer) where

import Data.Maybe
import Data.Functor.Identity
import qualified Data.Map as Map
import qualified Data.Set as Set
import Control.Monad.RWS
import Control.Monad.Except

import Syntax
import Substitution

type TEnv = Map.Map String (TypeScheme, Bool)
type Infer = RWST TEnv [Constraint] Int (Except String)

-- Inferring
fresh :: Infer Type
fresh = do
    n <- get
    put (n + 1)
    return . TVar . TV $ names !! n
    where
        names = map ('_' :) $ [1..] >>= flip replicateM ['a'..'z']

constrain :: Constraint -> Infer ()
constrain = tell . (: [])

generalize :: TEnv -> Type -> TypeScheme
generalize env t = Forall (Set.toList vs) t
    where vs = tvs t `Set.difference` tvs (map fst $ Map.elems env)

instantiate :: TypeScheme -> Infer Type
instantiate (Forall vs t) = do
    nvs <- traverse (const fresh) vs
    let s = Map.fromList (zip vs nvs)
    return $ apply s t

infer :: [UntypedDecl] -> Either String [TypedDecl]
infer decls =
    case runIdentity $ runExceptT $ runRWST (inferTopLevel decls []) Map.empty 0 of
        Left err -> Left err
        Right (p, _, cs) -> do
            s <- runSolve cs
            return $ fmap (fmap (apply s)) p

inferTopLevel :: [UntypedDecl] -> [(UntypedDecl, Maybe Type)] -> Infer [TypedDecl]
inferTopLevel [] [] = return []
inferTopLevel [] l = fst <$> inferDecls l [] []
inferTopLevel (DStmt _ : _) _ = throwError "No top-level statements allowed"
inferTopLevel (decl : rest) l =
    case decl of
        d@(DFunc _ name params tann expr) -> do
            ft <- inferTopLevelFn name params tann expr
            local (Map.insert name (Forall [] ft, False)) (inferTopLevel rest ((d, Just ft) : l))
        d@(DOper _ opdef op params tann expr) -> do
            ft <- inferTopLevelFn op params tann expr
            local (Map.insert op (Forall [] ft, False)) (inferTopLevel rest ((d, Just ft) : l))
        d@(DVar _ isMut name tann expr) -> throwError "Top-level variable declaration"
        {-    t <- fresh
            case tann of
                Nothing -> return ()
                Just ta -> constrain (CEqual ta t)
            local (Map.insert name (Forall [] t, isMut)) (inferTopLevel rest ((d, Just t) : l))
        -}
        DStmt (SRet _) -> throwError "Top-level return statement"
        DStmt _ -> undefined

inferTopLevelFn :: String -> Params -> TypeAnnot -> UntypedExpr -> Infer Type
inferTopLevelFn name params tann expr = do
    let (ps, panns) = unzip params
    pts <- traverse (const fresh) params
    sequence_ [constrain (CEqual pt pann) | (pt, pann) <- zip pts (filterNothings panns)]
    rt <- fresh
    case tann of
        Nothing -> return ()
        Just t -> constrain (CEqual t rt)
    return (TFunc pts rt)

-- TODO: clean
inferDecls :: [(UntypedDecl, Maybe Type)] -> [TypedDecl] -> [Type] -> Infer ([TypedDecl], [Type])
inferDecls [] l typs = return (l, typs)
inferDecls ((DStmt (SRet e), t) : rest) l typs = do
    (e', et) <- inferExpr e
    inferDecls rest (DStmt (SRet e') : l) (et : typs)
inferDecls ((DStmt s, t) : rest) l typs = do
    dstmt <- DStmt <$> inferStmt s
    inferDecls rest (dstmt : l) typs
inferDecls ((DFunc _ name params tann expr, t) : rest) l typs = do
    env <- ask
    ((expr', et), consts) <- listen (inferFn name params tann expr)
    subst <- liftEither (runSolve consts)
    let et' = apply subst et
        scheme = generalize env et'
    let (TFunc pts rt) = et'
    when (isJust tann) (constrain $ CEqual (fromJust tann) rt)
    let (_, panns) = unzip params
    sequence_ [constrain (CEqual pt pann) | (pt, pann) <- zip pts (filterNothings panns)]
    when (isJust t) (constrain $ CEqual et' (fromJust t))
    local (Map.insert name (scheme, False) . Map.delete name) (inferDecls rest (DFunc et' name params tann expr' : l) typs)
inferDecls ((DVar _ isMut name tann expr, t) : rest) l typs = do
    env <- ask
    when (isJust (Map.lookup name env)) (throwError $ "Already defined variable " ++ name)
    ((expr', et), consts) <- listen (inferExpr expr)
    subst <- liftEither (runSolve consts)
    let et' = apply subst et
        scheme = generalize env et'
    when (isJust tann) (constrain $ CEqual (fromJust tann) et')
    when (isJust t) (constrain $ CEqual et' (fromJust t))
    local (Map.insert name (scheme, isMut) . Map.delete name) (inferDecls rest (DVar et' isMut name tann expr' : l) typs)
inferDecls ((DOper _ opdef op params tann expr, t) : rest) l typs = do
    env <- ask
    ((expr', et), consts) <- listen (inferFn op params tann expr)
    subst <- liftEither (runSolve consts)
    let et' = apply subst et
        scheme = generalize env et'
    let (TFunc pts rt) = et'
    when (isJust tann) (constrain $ CEqual (fromJust tann) rt)
    let (_, panns) = unzip params
    sequence_ [constrain (CEqual pt pann) | (pt, pann) <- zip pts (filterNothings panns)]
    when (isJust t) (constrain $ CEqual et' (fromJust t))
    local (Map.insert op (scheme, False) . Map.delete op) (inferDecls rest (DOper et' opdef op params tann expr' : l) typs)

inferFn :: String -> Params -> TypeAnnot -> UntypedExpr -> Infer (TypedExpr, Type)
inferFn name params tann expr = do
    let (ps, panns) = unzip params
    pts <- traverse (const fresh) params
    let pts_ts = map ((,False) . Forall []) pts
    let nenv = Map.fromList (zip ps pts_ts)
    (e', rt) <- local (`Map.union` nenv) (inferExpr expr)
    return (e', TFunc pts rt)

inferStmt :: UntypedStmt -> Infer TypedStmt
inferStmt = \case
    SExpr e -> SExpr . fst <$> inferExpr e
    SWhile c b -> do
        (c', ct) <- inferExpr c
        (b', bt) <- inferExpr b
        constrain (CEqual ct TBool)
        return (SWhile c' b')
    SRet e -> throwError "Return statement outside of block but also not in top-level (?)"

inferExpr :: UntypedExpr -> Infer (TypedExpr, Type)
inferExpr = \case
    ELit _ l -> do
        let t = inferLit l
        return (ELit t l, t)
    EVar _ v -> lookupType v >>= \t -> return (EVar t v, t)
    EAssign _ a b -> do
        (a', at) <- inferExpr a
        case a' of
            EVar _ n -> do
                isMut <- lookupMut n
                if isMut then return ()
                else throwError $ "Cannot assign to immutable variable " ++ n
            EDeref _ _ -> return ()
            _ -> throwError "Cannot assign to non-lvalue"
        (b', bt) <- inferExpr b
        constrain (CEqual at bt)
        return (EAssign at a' b', at)
    EBlock _ decls res -> do
        (res', rt) <- inferExpr res
        (decls', ret_types) <- inferDecls (zip decls (repeat Nothing)) [] []
        case ret_types of
            [] -> do
                return (EBlock rt (reverse decls') res', rt)
            (t : rest) -> do
                sequence_ [constrain (CEqual t t') | t' <- rest]
                return (EBlock t (reverse decls') res', t)
    EIf _ c a b -> do
        (c', ct) <- inferExpr c
        (a', at) <- inferExpr a
        (b', bt) <- inferExpr b
        constrain (CEqual ct TBool)
        constrain (CEqual at bt)
        return (EIf at c' a' b', at)
    EMatch _ m bs -> do
        (m', mt) <- inferExpr m
        (bs', bts) <- unzip <$> traverse (inferBranch mt) bs
        case bts of
            [] -> throwError "Empty match expression"
            (bt : rest) -> (EMatch bt m' bs', bt) <$ mapM_ (constrain . CEqual bt) rest
    EBinOp _ op a b -> do
        (a', at) <- inferExpr a
        (b', bt) <- inferExpr b
        case op of
            "+" -> do -- TODO
                constrain (CEqual at TInt32)
                constrain (CEqual bt TInt32)
                return (EBinOp TInt32 op a' b', TInt32)
            _ -> do
                opt <- lookupType op
                rt <- fresh
                let ft = TFunc [at, bt] rt
                constrain (CEqual opt ft)
                return (ECall rt (EVar ft op) [a', b'], rt)
    EUnaOp _ op a -> do
        opt <- lookupType op
        (a', at) <- inferExpr a
        rt <- fresh
        constrain (CEqual opt (TFunc [at] rt))
        return (EUnaOp rt op a', rt)
    EClosure _ cvars params tann e -> do
        cvts <- map ((,False) . Forall []) <$> traverse lookupType cvars
        let (ps, panns) = unzip params
        pts <- traverse (const fresh) params
        sequence_ [constrain (CEqual pt pann) | (pt, pann) <- zip pts (filterNothings panns)]
        let pts_ts = map ((,False) . Forall []) pts
        let nenv = Map.fromList $ zip ps pts_ts ++ zip cvars cvts
        (e', rt) <- local (`Map.union` nenv) (inferExpr e)
        case tann of
            Nothing -> return ()
            Just t -> constrain (CEqual t rt)
        let ft = TFunc pts rt
        return (EClosure ft cvars params tann e', ft)
    ECall _ a bs -> do
        (a', at) <- inferExpr a
        (bs', bts) <- unzip <$> traverse inferExpr bs
        rt <- fresh
        constrain (CEqual at (TFunc bts rt))
        return (ECall rt a' bs', rt)
    ECast _ targ e -> do
        (e', et) <- inferExpr e
        case e' of
            ELit _ (LInt n) -> return (ECast targ targ e', targ)
            _ -> throwError $ "Invalid cast " ++ show et ++ " TO " ++ show targ
    EDeref _ e -> do
        (e', et) <- inferExpr e
        case et of
            TPtr t -> return (EDeref t e', t)
            _ -> throwError "Cannot dereference non-pointer"
    ERef _ e -> do
        (e', et) <- inferExpr e
        case e' of
            EVar _ s -> return (ERef (TPtr et) e', TPtr et)
            _ -> throwError "Cannot reference non-variable"
    ESizeof _ t -> do
        return (ESizeof TInt32 t, TInt32)

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
    (expr', et) <- local (Map.fromList vars' `Map.union`) (inferExpr expr)
    return ((pat, expr'), et)

inferPattern :: Pattern -> Infer (Type, [(String, TypeScheme)])
-- inferPattern (PCon name pats) = do
--     (pts, vars) <- unzip <$> traverse inferPattern pats
--     undefined
inferPattern (PVar name) = do
    ptype <- fresh
    pure (ptype, [(name, Forall [] ptype)])
inferPattern (PAs name pat) = do
    undefined
inferPattern (PLit lit) = return (inferLit lit, [])
inferPattern PWild = do
    ptype <- fresh
    return (ptype, [])

lookupVar :: String -> Infer (TypeScheme, Bool)
lookupVar name = do
    env <- ask
    case Map.lookup name env of
        Just v -> return v
        Nothing -> throwError ("Unknown variable " ++ name)

lookupType :: String -> Infer Type
lookupType name = lookupVar name >>= instantiate . fst

lookupMut :: String -> Infer Bool
lookupMut name = snd <$> lookupVar name

filterNothings :: [Maybe a] -> [a]
filterNothings (Just x : xs) = x : filterNothings xs
filterNothings (Nothing : xs) = filterNothings xs
filterNothings _ = []

-- Solving
type Solve = ExceptT String Identity

compose :: Substitution -> Substitution -> Substitution
compose a b = Map.map (apply a) b `Map.union` a

unify :: Type -> Type -> Solve Substitution
unify a b | a == b = return Map.empty
unify (TVar v) t = bind v t
unify t (TVar v) = bind v t
unify a@(TCon c1) b@(TCon c2)
    | c1 /= c2 = throwError $ "Type mismatch " ++ show a ++ " ~ " ++ show b
    | otherwise = return Map.empty
unify a@(TFunc pts rt) b@(TFunc pts2 rt2) = unifyMany (rt : pts) (rt2 : pts2)
unify a@(TPtr t) b@(TPtr t2) = unify t t2
unify a b = throwError $ "Type mismatch " ++ show a ++ " ~ " ++ show b

unifyMany :: [Type] -> [Type] -> Solve Substitution
unifyMany [] [] = return Map.empty
unifyMany (t1 : ts1) (t2 : ts2) =
  do su1 <- unify t1 t2
     su2 <- unifyMany (apply su1 ts1) (apply su1 ts2)
     return (su2 `compose` su1)
unifyMany t1 t2 = throwError $ "Type mismatch " ++ show (head t1) ++ " ~ " ++ show (head t2)

bind :: TVar -> Type -> Solve Substitution
bind v t
    | v `Set.member` tvs t = throwError $ "Infinite type " ++ show v ++ " ~ " ++ show t
    | otherwise = return $ Map.singleton v t 

solve :: Substitution -> [Constraint] -> Solve Substitution
solve s c =
    case c of
        [] -> return s
        (CEqual t1 t2 : cs) -> do
            s1 <- unify t1 t2
            let nsub = s1 `compose` s
            solve (s1 `compose` s) (apply s1 cs)
        (CClass t cls : cs) -> undefined

runSolve :: [Constraint] -> Either String Substitution
runSolve cs = runIdentity $ runExceptT $ solve Map.empty cs
