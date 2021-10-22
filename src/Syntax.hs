module Syntax where

import OperatorDef

data Decl a
    = DFunc String [(String, Type)] Type Expr
    | DOper OperatorDef
    | DVar Bool String Type (Expr a)
    | DStmt (Stmt a)

data Stmt a
    = SExpr (Expr a)

data Expr a
    = ELit a Lit
    | EIf a (Expr a) (Expr a) (Expr a)

data Lit
    = LInt64 Integer
    | LFloat64 Double
    | LBool Bool
    | LChar Char

data Type
    = TCon String
    | TFunc Type Type
    | TVar String
