module Substitution where

import qualified Data.Map as Map
import qualified Data.Set as Set

import Type

type Substitution = Map.Map TVar Type

class Substitutable a where
    tvs :: a -> Set.Set TVar
    apply :: Substitution -> a -> a

instance Substitutable Type where
    tvs (TCon _ tps) = tvs tps
    tvs (TVar tv) = Set.singleton tv
    tvs (TFunc a b) = tvs a `Set.union` tvs b
    tvs (TPtr t) = tvs t
    apply s t@(TVar tv) = Map.findWithDefault t tv s
    apply s (TFunc a b) = TFunc (apply s a) (apply s b)
    apply s (TCon c tps) = TCon c (apply s tps)
    apply s (TPtr t) = TPtr (apply s t)

instance Substitutable TypeScheme where
    tvs (Forall vs t) = tvs t `Set.difference` Set.fromList vs
    apply s (Forall vs t) = Forall vs (apply (foldr Map.delete s vs) t)

instance Substitutable Constraint where
    tvs (CEqual a b) = tvs a `Set.union` tvs b
    apply s (CEqual a b) = CEqual (apply s a) (apply s b)

instance Substitutable a => Substitutable [a] where
    tvs = foldr (Set.union . tvs) Set.empty
    apply = map . apply
