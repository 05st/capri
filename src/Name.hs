module Name where

import Data.Text (Text)

data Name = Qualified [Text] | Unqualified Text
    deriving (Show, Eq, Ord)

extractName :: Name -> Text
extractName (Qualified qual) = last qual
extractName (Unqualified name) = name
