module OperatorDef where

import qualified Data.Text as T

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
    , oper :: T.Text 
    } deriving (Show)
