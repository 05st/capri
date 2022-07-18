module Analyzer.Unify where

import qualified Data.Map as M
import qualified Data.Set as S
import Data.Text (Text, pack, unpack)
import Control.Monad.Except
import Control.Monad.State

import Analyzer.AnalyzerError
import Analyzer.Substitution
import Type
import SyntaxInfo

-- Unification

type Solve a = ExceptT AnalyzerError (State Int) a

fresh :: Solve Type
fresh = do
    count <- get
    put (count + 1)
    return . TVar . TV . pack . ("__" ++ ) $ supply !! count
    where
        supply = [1..] >>= flip replicateM ['a'..'z']

compose :: Substitution -> Substitution -> Substitution
compose a b = M.map (apply a) b `M.union` a

unify :: SyntaxInfo -> Type -> Type -> Solve Substitution
unify _ a b | a == b = return M.empty
unify pos (TVar v) t = bind pos v t
unify pos t (TVar v) = bind pos v t
unify pos a@(TConst c1) b@(TConst c2)
    | c1 /= c2 = throwError (GenericAnalyzerError pos ("Type mismatch " ++ show a ++ " ~ " ++ show b))
    | otherwise = return M.empty
unify pos (TApp a1 bs1) (TApp a2 bs2) = unifyMany pos (a1 : bs1) (a2 : bs2)
unify pos (TArrow ps r) (TArrow ps2 r2) = unifyMany pos (r : ps) (r2 : ps2)
unify pos a@(TRecordExtend label1 ty1 restRow1) b@TRecordExtend {} = do
    (restRow2, sub) <- rewriteRow pos b label1 ty1
    sub2 <- unify pos restRow1 restRow2
    return (sub2 `compose` sub)
unify pos (TPtr t1) (TPtr t2) = unify pos t1 t2
unify pos a b = throwError (GenericAnalyzerError pos ("Type mismatch " ++ show a ++ " ~ " ++ show b))

unifyMany :: SyntaxInfo -> [Type] -> [Type] -> Solve Substitution
unifyMany pos (t1 : ts1) (t2 : ts2) = do
    su1 <- unify pos t1 t2
    su2 <- unifyMany pos (apply su1 ts1) (apply su1 ts2)
    return (su2 `compose` su1)
unifyMany _ _ _ = return M.empty

rewriteRow :: SyntaxInfo -> Type -> Text -> Type -> Solve (Type, Substitution)
rewriteRow pos row2 label1 ty1 =
    case row2 of
        TRecordEmpty -> throwError (GenericAnalyzerError pos ("Row doesn't contain label '" ++ unpack label1 ++ "'"))
        TRecordExtend label2 ty2 restRow2 | label2 == label1 -> do
            su1 <- unify pos ty1 ty2
            return (apply su1 restRow2, su1)
        TRecordExtend label2 ty2 restRow2 -> do
            (recurseTyp, recurseSub) <- rewriteRow pos restRow2 label1 ty1
            return (apply recurseSub (TRecordExtend label2 ty2 recurseTyp), recurseSub)
        tv@TVar {} -> do
            restTv <- fresh
            su1 <- unify pos tv (TRecordExtend label1 ty1 restTv)
            return (apply su1 restTv, su1)
        _ -> throwError (GenericAnalyzerError pos "Row type expected")

bind :: SyntaxInfo -> TVar -> Type -> Solve Substitution
bind pos v t
    | v `S.member` ftv t = throwError (GenericAnalyzerError pos ("Infinite type " ++ show v ++ " ~ " ++ show t))
    | otherwise = return $ M.singleton v t 

solve :: Substitution -> [Constraint] -> Solve Substitution
solve s c =
    case c of
        [] -> return s
        (Constraint info t1 t2 : cs) -> do
            s1 <- unify info t1 t2
            let nsub = s1 `compose` s
            solve nsub (apply nsub cs)

runSolve :: [Constraint] -> Either AnalyzerError Substitution
runSolve cs = evalState (runExceptT $ solve M.empty cs) 0