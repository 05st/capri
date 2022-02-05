{-# Language OverloadedStrings #-}
{-# Language LambdaCase #-}

module Analyzer.Infer where

import qualified Data.Map as M
import Control.Monad.State
import Control.Monad.Except

import Text.Megaparsec.Pos (SourcePos)

import Syntax
import Type
import Name

import Analyzer.AnalyzerError

type Env = M.Map Name (Type, Bool)
type Infer = ExceptT AnalyzerError (State InferState)

data InferState = InferState
    { environment :: Env
    , freshCount :: Int
    , level :: Int
    } deriving (Show)

