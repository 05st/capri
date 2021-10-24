{-# Language PatternSynonyms #-}
{-# Language DeriveFunctor #-}
{-# Language OverloadedStrings #-}
{-# Language LambdaCase #-}
{-# Language TupleSections #-}

module Syntax where

import qualified Data.Text as T
import OperatorDef

type TypeAnnot = Maybe Type
type Params = [(T.Text, TypeAnnot)]
data Decl a
    = DFunc a T.Text Params TypeAnnot (Expr a)
    | DOper a OperatorDef T.Text Params TypeAnnot (Expr a)
    | DVar a Bool T.Text TypeAnnot (Expr a)
    | DExtern T.Text [Type] Type
    | DStmt (Stmt a)
    deriving (Show, Functor)

data Stmt a
    = SExpr (Expr a)
    | SRet (Expr a)
    | SWhile (Expr a) (Expr a)
    deriving (Show, Functor)

data Expr a
    = ELit a Lit
    | EVar a T.Text
    | EAssign a (Expr a) (Expr a)
    | EBlock a [Decl a] (Expr a)
    | EIf a (Expr a) (Expr a) (Expr a)
    | EMatch a (Expr a) [(Pattern, Expr a)]
    | EBinOp a T.Text (Expr a) (Expr a)
    | EUnaOp a T.Text (Expr a)
    | EClosure a [T.Text] Params TypeAnnot (Expr a)
    | ECall a (Expr a) [Expr a]
    | ECast a Type (Expr a)
    | EDeref a (Expr a)
    | ERef a (Expr a)
    | ESizeof a Type
    deriving (Show, Functor)

data Lit
    = LInt Integer
    | LFloat Double
    | LString T.Text
    | LChar Char
    | LBool Bool
    | LUnit
    deriving (Show)

newtype TVar = TV T.Text deriving (Show, Eq, Ord)
data Type
    = TCon T.Text
    | TFunc [Type] Type
    | TVar TVar
    | TPtr Type
    deriving (Show, Eq)

data Constraint = CEqual Type Type | CClass Type [T.Text] deriving (Show)
data TypeScheme = Forall [TVar] Type deriving (Show)

data Pattern
    = PLit Lit
    | PVar T.Text
    | PAs T.Text Pattern
    | PWild
    deriving (Show)

typeOfExpr :: TypedExpr -> Type
typeOfExpr = \case
    ELit t _ -> t
    EVar t _ -> t
    EAssign t _ _ -> t
    EBlock t _ _ -> t
    EIf t _ _ _ -> t
    EMatch t _ _ -> t
    EBinOp t _ _ _ -> t
    EUnaOp t _ _ -> t
    EClosure t _ _ _ _ -> t
    ECall t _ _ -> t
    ECast t _ _ -> t
    EDeref t _ -> t
    ERef t _ -> t
    ESizeof t _ -> t

type UntypedDecl = Decl ()
type TypedDecl = Decl Type
type UntypedStmt = Stmt ()
type TypedStmt = Stmt Type
type UntypedExpr = Expr ()
type TypedExpr = Expr Type

pattern TInt8 = TCon "i8"
pattern TInt16 = TCon "i16"
pattern TInt32 = TCon "i32"
pattern TInt64 = TCon "i64"

pattern TUInt8 = TCon "u8"
pattern TUInt16 = TCon "u16"
pattern TUInt32 = TCon "u32"
pattern TUInt64 = TCon "u64"

pattern TFloat32 = TCon "f32"
pattern TFloat64 = TCon "f64"

pattern TStr = TCon "str"
pattern TChar = TCon "char"
pattern TBool = TCon "bool"
pattern TUnit = TCon "unit"
