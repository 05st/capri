module SyntaxInfo where

import Text.Megaparsec.Pos

newtype SyntaxInfo = SyntaxInfo SourcePos deriving (Show)
