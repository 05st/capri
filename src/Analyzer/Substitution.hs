module Analyzer.Substitution where

import qualified Data.Map as M
import qualified Data.Set as S

import Type
import Syntax

type Substitution
    = M.Map TVar Type

class Substitutable a where
    ftv :: a -> S.Set TVar
    apply :: Substitution -> a -> a

instance Substitutable Type where
    ftv (TConst _) = S.empty
    ftv (TVar tv) = S.singleton tv
    ftv (TApp t1 ts) = ftv t1 `S.union` ftv ts
    ftv (TArrow ts rt) = ftv ts `S.union` ftv rt
    ftv TRecordEmpty = S.empty
    ftv (TRecordExtend _ typ rest) = ftv typ `S.union` ftv rest
    ftv (TPtr t) = ftv t
    apply s t@(TConst _) = t
    apply s t@(TVar tv) = M.findWithDefault t tv s
    apply s (TApp t1 ts) = apply s t1 `TApp` apply s ts
    apply s (TArrow ts rt) = apply s ts `TArrow` apply s rt
    apply s TRecordEmpty = TRecordEmpty
    apply s (TRecordExtend l typ rest) = TRecordExtend l (apply s typ) (apply s rest)
    apply s (TPtr t) = TPtr (apply s t)

instance Substitutable PolyType where
    ftv (Forall vs typ) = ftv typ `S.difference` S.fromList vs
    apply s (Forall vs typ) = Forall vs (apply (foldr M.delete s vs) typ)

instance Substitutable Constraint where
    ftv (Constraint _ a b) = ftv a `S.union` ftv b
    apply s (Constraint pos a b) = Constraint pos (apply s a) (apply s b)

instance Substitutable a => Substitutable [a] where
    ftv = foldr (S.union . ftv) S.empty
    apply = map . apply
