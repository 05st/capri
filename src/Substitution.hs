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
    tvs (TArray t _) = tvs t
    apply s t@(TVar tv) = Map.findWithDefault t tv s
    apply s (TCon c tps) = TCon c (apply s tps)
    apply s (TArray t l) = TArray (apply s t) l

instance Substitutable TypeScheme where
    tvs (Forall vs t) = tvs t `Set.difference` Set.fromList vs
    apply s (Forall vs t) = Forall vs (apply (foldr Map.delete s vs) t)

instance Substitutable Constraint where
    tvs (CEqual _ a b) = tvs a `Set.union` tvs b
    apply s (CEqual pos a b) = CEqual pos (apply s a) (apply s b)

instance Substitutable a => Substitutable [a] where
    tvs = foldr (Set.union . tvs) Set.empty
    apply = map . apply
