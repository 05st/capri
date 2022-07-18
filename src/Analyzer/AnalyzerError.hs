{-# Language OverloadedStrings #-}

module Analyzer.AnalyzerError where

import Data.List
import SyntaxInfo
import Text.Megaparsec.Pos (SourcePos, sourcePosPretty)

data AnalyzerError
    = GenericAnalyzerError SyntaxInfo String
    | CyclicDependencyError [String] 
    | NonexistentModulesError [String] -- add import source positions
    | UndefinedError SyntaxInfo String
    | RedefinitionError SyntaxInfo String

instance Show AnalyzerError where
    show (GenericAnalyzerError synInfo msg) = "ERROR: " ++ msg ++ " (" ++ sourcePosPretty (syntaxInfoSourcePos synInfo) ++ ")"
    show (CyclicDependencyError cycle) = "ERROR: Cyclic dependencies " ++ intercalate " -> " (cycle ++ [head cycle])
    show (NonexistentModulesError mods) = "ERROR: Nonexistent module(s) " ++ intercalate ", " mods
    show (UndefinedError synInfo name) = "ERROR: Undefined '" ++ name ++ "' (" ++ sourcePosPretty (syntaxInfoSourcePos synInfo) ++ ")"
    show (RedefinitionError synInfo name) = "ERROR: Redefinition of '" ++ name ++ "' (" ++ sourcePosPretty (syntaxInfoSourcePos synInfo) ++ ")"
