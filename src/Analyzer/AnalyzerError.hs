{-# Language OverloadedStrings #-}

module Analyzer.AnalyzerError where

import Data.List
import Text.Megaparsec.Pos (SourcePos, sourcePosPretty)

data AnalyzerError
    = GenericAnalyzerError SourcePos String
    | CyclicDependencyError [String] 
    | NonexistentModulesError [String] -- add import source positions
    | UndefinedError SourcePos String
    | RedefinitionError SourcePos String

instance Show AnalyzerError where
    show (GenericAnalyzerError pos msg) = "ERROR: " ++ msg ++ " (" ++ sourcePosPretty pos ++ ")"
    show (CyclicDependencyError cycle) = "ERROR: Cyclic dependencies " ++ intercalate " -> " (cycle ++ [head cycle])
    show (NonexistentModulesError mods) = "ERROR: Nonexistent module(s) " ++ intercalate ", " mods
    show (UndefinedError pos name) = "ERROR: " ++ "Undefined '" ++ name ++ "' (" ++ sourcePosPretty pos ++ ")"
    show (RedefinitionError pos name) = "ERROR: " ++ "Redefinition of '" ++ name ++ "' (" ++ sourcePosPretty pos ++ ")"
