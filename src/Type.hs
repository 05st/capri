{-# Language LambdaCase #-}
{-# Language OverloadedStrings #-}
{-# Language PatternSynonyms #-}

module Type where

import Data.Text (Text, unpack)
import Data.List

import Text.Megaparsec.Pos

import Name

newtype TVar = TV Text deriving (Show, Eq, Ord)
data Type
    = TCon Name [Type]
    | TVar TVar
    deriving (Eq, Ord)

data Constraint = CEqual SourcePos Type Type deriving (Show)
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

pattern TFunc pts rt = TCon (Unqualified "->") (rt : pts)
pattern TPtr t = TCon (Unqualified "*") [t]
pattern TArray t = TCon (Unqualified "[]") [t]

instance Show Type where
    show = \case
        TPtr t -> show t ++ "*"
        TArray t -> "[]" ++ show t
        TFunc pts rt -> intercalate ", " (map show pts) ++ " -> " ++ show rt
        TCon con [] -> show con
        TCon con ts -> show con ++ "<" ++ intercalate ", " (map show ts) ++ ">"
        TVar (TV var) -> unpack var
