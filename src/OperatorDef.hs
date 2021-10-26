module OperatorDef where

import Data.Text (Text)

data Assoc
    = ALeft
    | ARight
    | ANone
    | APrefix
    | APostfix
    deriving (Show)

data OperatorDef = OperatorDef
    { assoc :: Assoc
    , prec :: Integer
    , oper :: Text 
    } deriving (Show)
