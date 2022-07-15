{-# Language DeriveDataTypeable #-}

module SyntaxInfo where

import Text.Megaparsec.Pos
import Data.Data

newtype SyntaxInfo = SyntaxInfo SourcePos deriving (Show, Data)

syntaxInfoSourcePos :: SyntaxInfo -> SourcePos
syntaxInfoSourcePos (SyntaxInfo pos) = pos
