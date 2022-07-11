{-# Language PatternSynonyms #-}
{-# Language OverloadedStrings #-}
{-# Language DeriveDataTypeable #-}

module Type where

import Data.Text (Text, unpack)
import Data.List
import Data.Data
import Text.Megaparsec (SourcePos)

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
    deriving (Eq, Data, Ord)
-- | TRowExtend (LabelMap Type) Row

newtype TVar
    = TV Text
    deriving (Eq, Ord, Data)

data PolyType
    = Forall [TVar] Type
    deriving (Show)

{-
data TVar
    = TVPlain Text
    | TVUnbound Text Int
    | TVLink Type
    | TGeneric Text
    deriving (Show)
-}

data Constraint
    = Constraint SourcePos Type Type
    deriving (Show)

pattern TInt8 = TConst (Unqualified "i8")
pattern TInt16 = TConst (Unqualified "i16")
pattern TInt32 = TConst (Unqualified "i32")
pattern TInt64 = TConst (Unqualified "i64")
pattern TUInt8 = TConst (Unqualified "u8")
pattern TUInt16 = TConst (Unqualified "u16")
pattern TUInt32 = TConst (Unqualified "u32")
pattern TUInt64 = TConst (Unqualified "u64")
pattern TFloat32 = TConst (Unqualified "f32")
pattern TFloat64 = TConst (Unqualified "f64")
pattern TChar = TConst (Unqualified "char")
pattern TString = TConst (Unqualified "str")
pattern TBool = TConst (Unqualified "bool")
pattern TUnit = TConst (Unqualified "unit")

instance Show Type where
    show (TConst name) = show name
    show (TVar tv) = show tv
    show (TApp a bs) = show a ++ "<" ++ intercalate ", " (map show bs) ++ ">"
    show (TArrow ps rt) = intercalate ", " (map show ps) ++ " -> " ++ show rt
    show (TRecord row) = "{" ++ show row ++ "}"
    show (TVariant row) = "<" ++ show row ++ ">"
    show TRowEmpty = ""
    show (TRowExtend l t TRowEmpty) = unpack l ++ ": " ++ show t
    show (TRowExtend l t r) = unpack l ++ ": " ++ show t ++ ", " ++ show r

instance Show TVar where
    show (TV name) = unpack name