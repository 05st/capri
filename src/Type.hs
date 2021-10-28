{-# Language LambdaCase #-}
{-# Language OverloadedStrings #-}
{-# Language PatternSynonyms #-}

module Type where

import Data.Text (Text)

newtype TVar = TV Text deriving (Show, Eq, Ord)
data Type
    = TCon Text [Type]
    | TFunc [Type] Type
    | TVar TVar
    | TPtr Type
    deriving (Show, Eq)

data Constraint = CEqual Type Type deriving (Show)
data TypeScheme = Forall [TVar] Type deriving (Show)

pattern TInt8 = TCon "i8" []
pattern TInt16 = TCon "i16" []
pattern TInt32 = TCon "i32" []
pattern TInt64 = TCon "i64" []

pattern TUInt8 = TCon "u8" [] 
pattern TUInt16 = TCon "u16" []
pattern TUInt32 = TCon "u32" []
pattern TUInt64 = TCon "u64" []

pattern TFloat32 = TCon "f32" []
pattern TFloat64 = TCon "f64" []

pattern TStr = TCon "str" []
pattern TChar = TCon "char" []
pattern TBool = TCon "bool" []
pattern TUnit = TCon "unit" []
