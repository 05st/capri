module Type where

import Data.Text (Text)

import Name

type Row = Type
data Type
    = TConst Name
    | TVar TVar
    | TApp Type [Type]
    | TArrow [Type] Type
    | TRecord Row
    | TVariant Row
    | TRowEmpty
    | TRowExtend Text Type Row
    deriving (Show)

data TVar
    = TVPlain Text
    | TVUnbound Text Int
    | TVLink Type
    | TGeneric Text
    deriving (Show)
