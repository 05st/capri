{-# Language LambdaCase #-}

module Infer where

import qualified Data.Map as Map
import qualified Data.Set as Set
import Control.Monad.RWS
import Control.Monad.Except

import Syntax
import Substitution

type TEnv = Map.Map String TypeScheme
type Infer = RWST (Map.Map String TypeScheme) [Constraint] Int (Except String)

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
    where vs = tvs t `Set.difference` tvs (Map.elems env)

instantiate :: TypeScheme -> Infer Type
instantiate (Forall vs t) = do
    nvs <- traverse (const fresh) vs
    let s = Map.fromList (zip vs nvs)
    return $ apply s t

infer :: [UntypedDecl] -> Either String [TypedDecl]
infer = undefined

inferDecl :: UntypedDecl -> Infer TypedDecl
inferDecl = undefined

inferExpr :: UntypedExpr -> Infer (TypedExpr, Type)
inferExpr = \case
    ELit _ l -> do
        let t = inferLit l
        return (ELit t l, t)
    EVar _ v -> lookupType v >>= \t -> return (EVar t v, t)
    EAssign _ a b -> do
        (a', at) <- inferExpr a
        (b', bt) <- inferExpr b
        constrain (CEqual at bt)
        return (EAssign at a' b', at)
    EBlock _ decls res -> do
        decls' <- traverse inferDecl decls
        (res', rt) <- inferExpr res
        return (EBlock rt decls' res', rt)
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
        opt <- lookupType op
        (a', at) <- inferExpr a
        (b', bt) <- inferExpr b
        rt <- fresh
        constrain (CEqual opt (TFunc [at, bt] rt))
        return (EBinOp rt op a' b', rt)
    EUnaOp _ op a -> do
        opt <- lookupType op
        (a', at) <- inferExpr a
        rt <- fresh
        constrain (CEqual opt (TFunc [at] rt))
        return (EUnaOp rt op a', rt)
    EClosure _ cvars params tann e -> do
        cvts <- map (Forall []) <$> traverse lookupType cvars
        let (ps, panns) = unzip params
        pts <- traverse (const fresh) params
        sequence_ [constrain (CEqual pt pann) | pt <- pts, pann <- filterNothings panns]
        let pts_ts = map (Forall []) pts
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

inferLit :: Lit -> Type
inferLit = \case
    LInt _ -> TInt32
    LFloat _ -> TFloat32
    LString _ -> TStr
    LChar _ -> TChar
    LBool _ -> TBool
    LUnit -> TUnit

inferBranch :: Type -> (Pattern, UntypedExpr) -> Infer ((Pattern, TypedExpr), Type)
inferBranch mt (pat, expr) = do
    (pt, vars) <- inferPattern pat
    constrain (CEqual pt mt)
    (expr', et) <- local (Map.fromList vars `Map.union`) (inferExpr expr)
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

lookupType :: String -> Infer Type
lookupType name = do
    env <- ask
    case Map.lookup name env of
        Just t -> instantiate t
        Nothing -> throwError ("Unknown variable " ++ name)

filterNothings :: [Maybe a] -> [a]
filterNothings (Just x : xs) = x : filterNothings xs
filterNothings (Nothing : xs) = filterNothings xs
filterNothings _ = []
