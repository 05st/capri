module Name where

import Data.Text (Text, unpack)

data Name
    = Qualified [Text] Text
    | Unqualified Text

instance Show Name where
    show (Qualified quals name) = concatMap ((++ "::") . unpack) quals ++ unpack name
    show (Unqualified name) = unpack name
