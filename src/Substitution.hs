module Substitution where

import qualified Data.Map as Map
import qualified Data.Set as Set

import Syntax

type Substitution = Map.Map TVar Type

class Substitutable a where
    tvs :: a -> Set.Set TVar
    apply :: Substitution -> a -> a

instance Substitutable Type where
    tvs (TCon _) = Set.empty
    tvs (TVar tv) = Set.singleton tv
    tvs (TFunc a b) = tvs a `Set.union` tvs b
    tvs (TPtr t) = tvs t
    tvs (TArr t _) = tvs t
    apply s t@(TVar tv) = Map.findWithDefault t tv s
    apply s (TFunc a b) = TFunc (apply s a) (apply s b)
    apply s t@(TCon _) = t
    apply s (TPtr t) = TPtr (apply s t)
    apply s (TArr t l) = TArr (apply s t) l

instance Substitutable TypeScheme where
    tvs (Forall vs t) = tvs t `Set.difference` Set.fromList vs
    apply s (Forall vs t) = Forall vs (apply (foldr Map.delete s vs) t)

instance Substitutable Constraint where
    tvs (CEqual a b) = tvs a `Set.union` tvs b
    tvs (CClass t _) = tvs t
    apply s (CEqual a b) = CEqual (apply s a) (apply s b)
    apply s (CClass t c) = CClass (apply s t) c

instance Substitutable a => Substitutable [a] where
    tvs = foldr (Set.union . tvs) Set.empty
    apply = map . apply
