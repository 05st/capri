{-# Language PatternSynonyms #-}
{-# Language DeriveFunctor #-}
{-# Language OverloadedStrings #-}
{-# Language LambdaCase #-}
{-# Language TupleSections #-}
{-# Language DeriveDataTypeable #-}

module Syntax where

import Data.Text (Text)
import Data.Data

import Text.Megaparsec.Pos

import OperatorDef
import Type
import Name

type Program a = [Module a]

type Import = [Text]
data Module a = Module
    { modSourcePos :: SourcePos
    , modFullName :: [Text]
    , modImports :: [Import]
    , modTopLvls :: [TopLvl a]
    , modPubs :: [Text]
    } deriving (Show, Functor)

type TypeAnnot = Maybe Type
type Params = [(Text, TypeAnnot)]
data TopLvl a
    = TLFunc a SourcePos Name Params TypeAnnot (Expr a)
    | TLOper a SourcePos Name Params TypeAnnot (Expr a)
    | TLExtern Text [Type] Type
    | TLType SourcePos Name [TVar] [(Name, [Type])]
    | TLStruct SourcePos Name [TVar] [(Text, Type)]
    deriving (Show, Functor)

data Decl a
    = DVar a SourcePos Bool Text TypeAnnot (Expr a)
    | DStmt (Stmt a)
    deriving (Show, Functor, Data, Typeable)

data Stmt a
    = SExpr (Expr a)
    | SRet (Expr a)
    | SWhile SourcePos (Expr a) (Expr a)
    deriving (Show, Functor, Data, Typeable)

data Expr a
    = ELit a SourcePos Lit
    | EVar a SourcePos [a] Name
    | EAssign a SourcePos (Expr a) (Expr a)
    | EBlock a SourcePos [Decl a] (Expr a)
    | EIf a SourcePos (Expr a) (Expr a) (Expr a)
    | EMatch a SourcePos (Expr a) [(Pattern, Expr a)]
    | EBinOp a SourcePos Name (Expr a) (Expr a)
    | EUnaOp a SourcePos Name (Expr a)
    | EClosure a SourcePos [Text] Params TypeAnnot (Expr a)
    | ECall a SourcePos (Expr a) [Expr a]
    | ECast a SourcePos Type (Expr a)
    | EDeref a SourcePos (Expr a)
    | ERef a SourcePos (Expr a)
    | EArrow a SourcePos (Expr a) Text
    | ESizeof a SourcePos (Either Type (Expr a))
    | EArray a SourcePos [Expr a]
    | EIndex a SourcePos (Expr a) Int
    | EStruct a SourcePos Name [(Text, Expr a)]
    | EAccess a SourcePos (Expr a) Text
    deriving (Show, Functor, Data, Typeable)

data Lit
    = LInt Integer
    | LFloat Double
    | LString Text
    | LChar Char
    | LBool Bool
    | LUnit
    deriving (Show, Data, Typeable)

data Pattern
    = PCon Name [Text]
    | PLit Lit
    | PVar Text
    | PWild
    deriving (Show, Data, Typeable)

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
    ELit t _ _ -> t
    EVar t _ _ _ -> t
    EAssign t _ _ _ -> t
    EBlock t _ _ _ -> t
    EIf t _ _ _ _ -> t
    EMatch t _ _ _ -> t
    EBinOp t _ _ _ _ -> t
    EUnaOp t _ _ _ -> t
    EClosure t _ _ _ _ _ -> t
    ECall t _ _ _ -> t
    ECast t _ _ _ -> t
    EDeref t _ _ -> t
    ERef t _ _ -> t
    ESizeof t _ _ -> t
    EArray t _ _ -> t
    EIndex t _ _ _ -> t
    EStruct t _ _ _ -> t
    EAccess t _ _ _ -> t
    EArrow t _ _ _ -> t
