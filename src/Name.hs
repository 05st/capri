{-# Language DeriveDataTypeable #-}

module Name where

import Data.Text (Text, unpack)
import Data.Data

data Name
    = Qualified [Text] Text
    | Unqualified Text
    deriving (Eq, Ord, Data)

extractName :: Name -> Text
extractName (Qualified _ n) = n
extractName (Unqualified n) = n

instance Show Name where
    show (Qualified quals name) = concatMap ((++ "::") . unpack) quals ++ unpack name
    show (Unqualified name) = unpack name
