{-# Language LambdaCase #-}
{-# Language OverloadedStrings #-}
{-# Language PatternSynonyms #-}

module Type where

import Data.Text (Text, unpack)
import Data.List

import Name

newtype TVar = TV Text deriving (Show, Eq, Ord)
data Type
    = TCon Name [Type]
    | TFunc [Type] Type
    | TVar TVar
    | TPtr Type
    | TArray Type
    deriving (Eq, Ord)

data Constraint = CEqual Type Type deriving (Show)
data TypeScheme = Forall [TVar] Type deriving (Show)

pattern TInt8 = TCon (Unqualified "i8") []
pattern TInt16 = TCon (Unqualified "i16") []
pattern TInt32 = TCon (Unqualified "i32") []
pattern TInt64 = TCon (Unqualified "i64") []

pattern TUInt8 = TCon (Unqualified "u8") [] 
pattern TUInt16 = TCon (Unqualified "u16") []
pattern TUInt32 = TCon (Unqualified "u32") []
pattern TUInt64 = TCon (Unqualified "u64") []

pattern TFloat32 = TCon (Unqualified "f32") []
pattern TFloat64 = TCon (Unqualified "f64") []

pattern TStr = TCon (Unqualified "str") []
pattern TChar = TCon (Unqualified "char") []
pattern TBool = TCon (Unqualified "bool") []
pattern TUnit = TCon (Unqualified "unit") []

instance Show Type where
    show = \case
        TCon con [] -> show con
        TCon con ts -> show con ++ "<" ++ intercalate ", " (map show ts) ++ ">"
        TFunc pts rt -> intercalate ", " (map show pts) ++ " -> " ++ show rt
        TVar (TV var) -> unpack var
        TPtr t -> show t ++ "*"
        TArray t -> show t ++ "[]"
