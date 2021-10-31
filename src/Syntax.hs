{-# Language PatternSynonyms #-}
{-# Language DeriveFunctor #-}
{-# Language OverloadedStrings #-}
{-# Language LambdaCase #-}
{-# Language TupleSections #-}

module Syntax where

import Data.Text (Text)

import OperatorDef
import Type
import Name

type Program a = [Module a]

type Import = [Text]
data Module a = Module [Text] [Import] [TopLvl a] [Text]
    deriving (Show, Functor)

type TypeAnnot = Maybe Type
type Params = [(Text, TypeAnnot)]
data TopLvl a
    = TLFunc a Name Params TypeAnnot (Expr a)
    | TLOper a OperatorDef Name Params TypeAnnot (Expr a)
    | TLExtern Text [Type] Type
    | TLType Name [TVar] [(Name, [Type])]
    | TLStruct 
    deriving (Show, Functor)

data Decl a
    = DVar a Bool Text TypeAnnot (Expr a)
    | DStmt (Stmt a)
    deriving (Show, Functor)

data Stmt a
    = SExpr (Expr a)
    | SRet (Expr a)
    | SWhile (Expr a) (Expr a)
    deriving (Show, Functor)

data Expr a
    = ELit a Lit
    | EVar a [a] Name
    | EAssign a (Expr a) (Expr a)
    | EBlock a [Decl a] (Expr a)
    | EIf a (Expr a) (Expr a) (Expr a)
    | EMatch a (Expr a) [(Pattern, Expr a)]
    | EBinOp a Name (Expr a) (Expr a)
    | EUnaOp a Name (Expr a)
    | EClosure a [Text] Params TypeAnnot (Expr a)
    | ECall a (Expr a) [Expr a]
    | ECast a Type (Expr a)
    | EDeref a (Expr a)
    | ERef a (Expr a)
    | ESizeof a (Either Type (Expr a))
    | EArray a [Expr a]
    | EIndex a (Expr a) Int
    deriving (Show, Functor)

data Lit
    = LInt Integer
    | LFloat Double
    | LString Text
    | LChar Char
    | LBool Bool
    | LUnit
    deriving (Show)

data Pattern
    = PCon Name [Text]
    | PLit Lit
    | PVar Text
    | PWild
    deriving (Show)

type UntypedProgram = Program ()
type TypedProgram = Program Type
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
    EVar t _ _ -> t
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
    EArray t _ -> t
    EIndex t _ _ -> t
