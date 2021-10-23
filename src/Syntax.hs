{-# Language PatternSynonyms #-}

module Syntax where

import OperatorDef

type TypeAnnot = Maybe Type
type Params = [(String, TypeAnnot)]
data Decl a
    = DFunc String Params TypeAnnot (Expr a)
    | DOper OperatorDef String Params TypeAnnot (Expr a)
    | DVar Bool String TypeAnnot (Expr a)
    | DStmt (Stmt a)
    deriving (Show)

data Stmt a
    = SExpr (Expr a)
    | SWhile (Expr a) (Expr a)
    deriving (Show)

data Expr a
    = ELit a Lit
    | EVar a String
    | EAssign a (Expr a) (Expr a)
    | EBlock a [Decl a] (Expr a)
    | EIf a (Expr a) (Expr a) (Expr a)
    | EMatch a (Expr a) [(Pattern, Expr a)]
    | EBinOp a String (Expr a) (Expr a)
    | EUnaOp a String (Expr a)
    | EClosure a [String] Params TypeAnnot (Expr a)
    | ECall a (Expr a) [Expr a]
    deriving (Show)

data Lit
    = LInt Integer
    | LFloat Double
    | LString String
    | LChar Char
    | LBool Bool
    | LUnit
    deriving (Show)

data Type
    = TCon String
    | TFunc [Type] Type
    | TVar String
    deriving (Show)

data Pattern
    = PLit Lit
    | PVar String
    | PAs String Pattern
    | PWild
    deriving (Show)

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
pattern TBool = TCon "bool"
pattern TUnit = TCon "unit"
