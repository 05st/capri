module Analyzer.Substitution where

import qualified Data.Map as M
import qualified Data.Set as S

import Type
import Syntax

type Substitution
    = M.Map TVar Type

class Substitutable a where
    tvs :: a -> S.Set TVar
    apply :: Substitution -> a -> a

instance Substitutable Type where
    tvs (TConst _) = S.empty
    tvs (TVar tv) = S.singleton tv
    tvs (TApp t1 ts) = tvs t1 `S.union` tvs ts
    tvs (TArrow ts rt) = tvs ts `S.union` tvs rt
    tvs (TRecord row) = tvs row
    tvs (TVariant row) = tvs row
    tvs TRowEmpty = S.empty
    tvs (TRowExtend _ typ row) = tvs typ `S.union` tvs row
    tvs (TPtr t) = tvs t
    apply s t@(TConst _) = t
    apply s t@(TVar tv) = M.findWithDefault t tv s
    apply s (TApp t1 ts) = apply s t1 `TApp` apply s ts
    apply s (TArrow ts rt) = apply s ts `TArrow` apply s rt
    apply s (TRecord row) = TRecord (apply s row)
    apply s (TVariant row) = TVariant (apply s row)
    apply s TRowEmpty = TRowEmpty
    apply s (TRowExtend l typ row) = TRowExtend l (apply s typ) (apply s row)
    apply s (TPtr t) = TPtr (apply s t)

instance Substitutable PolyType where
    tvs (Forall vs typ) = tvs typ `S.difference` S.fromList vs
    apply s (Forall vs typ) = Forall vs (apply (foldr M.delete s vs) typ)

instance Substitutable Constraint where
    tvs (Constraint _ a b) = tvs a `S.union` tvs b
    apply s (Constraint pos a b) = Constraint pos (apply s a) (apply s b)

instance Substitutable a => Substitutable [a] where
    tvs = foldr (S.union . tvs) S.empty
    apply = map . apply