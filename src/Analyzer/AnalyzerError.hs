{-# Language OverloadedStrings #-}
{-# Language LambdaCase #-}

module Analyzer.AnalyzerError where

import Data.List
import Text.Megaparsec.Pos (SourcePos, sourcePosPretty)

data AnalyzerError
    = GenericAnalyzerError SourcePos String
    | CyclicDependencyError [String] 
    | NonexistentModules [String] -- add import source positions

instance Show AnalyzerError where
    show (GenericAnalyzerError pos msg) = "ERROR: " ++ msg ++ " (" ++ sourcePosPretty pos ++ ")"
    show (CyclicDependencyError cycle) = "ERROR: Cyclic dependencies " ++ intercalate " -> " (cycle ++ [head cycle])
    show (NonexistentModules mods) = "ERROR: Nonexistent module(s) " ++ intercalate ", " mods
