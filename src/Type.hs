{-# Language PatternSynonyms #-}
{-# Language OverloadedStrings #-}
{-# Language DeriveDataTypeable #-}

module Type where

import Data.Text (Text, unpack)
import Data.List
import Data.Data
import Text.Megaparsec (SourcePos)

import Name
import SyntaxInfo

data Kind
    = KStar
    | KArrow Kind Kind

data Type
    = TConst        Name
    | TVar          TVar
    | TApp          Type [Type]
    | TArrow        [Type] Type
    | TRecordExtend Text Type Type
    | TRecordEmpty
    | TPtr          Type
    deriving (Eq, Data, Ord)

newtype TVar
    = TV Text
    deriving (Eq, Ord, Data)

data PolyType
    = Forall [TVar] Type
    deriving (Show)

data Constraint
    = Constraint SyntaxInfo Type Type
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

instance Show Kind where
    show KStar = "*"
    show (KArrow k1 k2) = "(" ++ show k1 ++ ") -> (" ++ show k2 ++ ")"

instance Show Type where
    show (TConst name) = show name
    show (TVar tv) = show tv
    show (TApp a bs) = show a ++ "<" ++ intercalate ", " (map show bs) ++ ">"
    show (TArrow ps rt) = intercalate ", " (map show ps) ++ " -> " ++ show rt
    show TRecordEmpty = "{}"
    show t@TRecordExtend {} =
        let fields = collectRecordTypeFields t
        in "{" ++ (intercalate ", " [unpack l ++ ": " ++ show t | (l, t) <- fields]) ++ "}"
    show (TPtr t) = show t ++ "*"

instance Show TVar where
    show (TV name) = unpack name

collectRecordTypeFields :: Type -> [(Text, Type)]
collectRecordTypeFields (TRecordExtend l t r) = (l, t) : collectRecordTypeFields r
collectRecordTypeFields _ = []