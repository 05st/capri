{-# Language OverloadedStrings #-}
{-# Language LambdaCase #-}

module Analyzer.AnalyzerError where

import Text.Megaparsec.Pos (SourcePos)

data AnalyzerError
    = GenericAnalyzerError SourcePos String
    | CyclicDependencyError [String] 
    | NonexistentModules [String] -- add import source positions
    deriving (Show)
