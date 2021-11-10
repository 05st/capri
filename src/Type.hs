module Type where

import Data.Text (Text)

import Name

data Type
    = TConst Name
    | TVar TVar
    | TApp Type Type
    | TArrow [Type] Type
    deriving (Show)

newtype TVar = TV Text deriving (Show)
