module SyntaxInfo where

import Text.Megaparsec.Pos

newtype SyntaxInfo = SyntaxInfo SourcePos deriving (Show)

syntaxInfoSourcePos :: SyntaxInfo -> SourcePos
syntaxInfoSourcePos (SyntaxInfo pos) = pos
