{-# Language PatternSynonyms #-}
{-# Language DeriveFunctor #-}
{-# Language OverloadedStrings #-}
{-# Language LambdaCase #-}
{-# Language TupleSections #-}

module Syntax where

import qualified Data.Text as T

import OperatorDef
import Type

type TypeAnnot = Maybe Type
type Params = [(T.Text, TypeAnnot)]

type Module a = [TopLvl a]

data TopLvl a
    = TLFunc a T.Text Params TypeAnnot (Expr a)
    | TLOper a OperatorDef T.Text Params TypeAnnot (Expr a)
    | TLExtern T.Text [Type] Type
    deriving (Show, Functor)

data Decl a
    = DVar a Bool T.Text TypeAnnot (Expr a)
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
    | ESizeof a (Either Type (Expr a))
    deriving (Show, Functor)

data Lit
    = LInt Integer
    | LFloat Double
    | LString T.Text
    | LChar Char
    | LBool Bool
    | LUnit
    deriving (Show)

data Pattern
    = PLit Lit
    | PVar T.Text
    | PWild
    deriving (Show)

type UntypedModule = Module ()
type TypedModule = Module Type
type UntypedTopLvl = TopLvl ()
type TypedTopLvl = TopLvl Type
type UntypedDecl = Decl ()
type TypedDecl = Decl Type
type UntypedStmt = Stmt ()
type TypedStmt = Stmt Type
type UntypedExpr = Expr ()
type TypedExpr = Expr Type

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
