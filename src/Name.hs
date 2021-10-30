module Name where

import Data.Text (Text, unpack)
import Data.List

data Name = Qualified [Text] | Unqualified Text
    deriving (Eq, Ord)

extractName :: Name -> Text
extractName (Qualified qual) = last qual
extractName (Unqualified name) = name

instance Show Name where
    show (Qualified quals) = intercalate "::" (map unpack quals)
    show (Unqualified name) = unpack name
