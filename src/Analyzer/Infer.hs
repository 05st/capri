{-# Language OverloadedStrings #-}
{-# Language LambdaCase #-}

module Analyzer.Infer where

import Data.Text (Text, pack)
import qualified Data.Map as M
import qualified Data.Set as S
import Control.Monad.State
import Control.Monad.Except

import Text.Megaparsec.Pos (SourcePos)

import Syntax
import Type
import Name

import Analyzer.AnalyzerError
import Analyzer.Substitution

import Debug.Trace

type Env = M.Map Name PolyType -- We don't need to keep track if they are public, the resolver took care of that
type Infer = ExceptT AnalyzerError (State InferState)
data InferState = InferState
    { environment :: Env
    , typeAliasEnv :: M.Map Name Type
    , preTopLvlEnv :: M.Map Name TVar
    , freshCount :: Int
    } deriving (Show)

inferProgram :: UntypedProgram -> Either AnalyzerError TypedProgram
inferProgram prog = evalState (runExceptT (preInference prog *> traverse inferModule prog)) initInferState
    where
        initInferState = InferState { environment = M.empty, typeAliasEnv = M.empty, preTopLvlEnv = M.empty, freshCount = 0 }

preInference :: UntypedProgram -> Infer ()
preInference prog = mapM_ initializeTopLvl (concatMap modTopLvls prog)
    where
        initializeTopLvl (TLFunc _ _ _ name _ _ _) = do
            env <- gets preTopLvlEnv
            state <- get
            typeVar <- fresh
            let TVar tv = typeVar
            put (state { preTopLvlEnv = M.insert name tv env })
        initializeTopLvl (TLType _ isPub name tvars typ) = do
            env <- gets typeAliasEnv
            state <- get
            put (state { typeAliasEnv = M.insert name typ env})
        initializeTopLvl TLExtern {} = undefined

inferModule :: UntypedModule -> Infer TypedModule
inferModule mod = do
    env <- gets environment
    trace (show env) $ return ()
    undefined

inferExpr :: UntypedExpr -> Infer TypedExpr
inferExpr = undefined

fresh :: Infer Type
fresh = do
    state <- get
    count <- gets freshCount
    put (state { freshCount = count + 1 })
    return . TVar . TV . pack . ('_':) $ varNames !! count
    where
        varNames = [1..] >>= flip replicateM ['a'..'z']

generalize :: Env -> Type -> PolyType
generalize env typ = Forall (S.toList vs) typ
    where
        vs = tvs typ `S.difference` tvs (M.elems env)

instantiate :: PolyType -> Infer Type
instantiate (Forall vs typ) = do
    nvs <- traverse (const fresh) vs
    let sub = M.fromList (zip vs nvs)
    return (apply sub typ)
