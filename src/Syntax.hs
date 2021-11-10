module Syntax where

import Data.Text (Text)

import SyntaxInfo
import Type
import Name

data TopLvl a
    = TLFunc SyntaxInfo Bool Name Params TypeAnnot (Expr a)
    | TLType SyntaxInfo Name [TVar] Type
    | TLExtern Text [Type] Type
    deriving (Show)
    
data Decl a
    = DStmt (Stmt a)
    | DVar SyntaxInfo Bool Text TypeAnnot (Expr a)
    deriving (Show)

data Stmt a
    = SExpr (Expr a)
    | SRet (Expr a)
    | SWhile SyntaxInfo (Expr a) (Expr a)
    deriving (Show)

data Expr a
    = ELit SyntaxInfo a Lit
    | EVar SyntaxInfo a [Type] Name
    | EAssign SyntaxInfo a (Expr a) (Expr a)
    | EBlock SyntaxInfo a [Decl a] (Expr a)
    | EIf SyntaxInfo a (Expr a) (Expr a) (Expr a)
    | EMatch SyntaxInfo a (Expr a) [(Pattern, Expr a)]
    | EBinOp SyntaxInfo a Name (Expr a) (Expr a)
    | EUnaOp SyntaxInfo a Name (Expr a)
    | EClosure SyntaxInfo a [Text] Params TypeAnnot (Expr a)
    | ECall SyntaxInfo a (Expr a) [Expr a]
    | ECast SyntaxInfo Type (Expr a)
    deriving (Show)

type TypeAnnot = Maybe Type
type Params = [(Text, TypeAnnot)]

data Lit
    = LInt Integer
    | LFloat Double
    | LString Text
    | LChar Char
    | LBool Bool
    | LUnit
    deriving (Show)

data Pattern
    = PVariant Name Pattern
    | PLit Lit
    | PVar Text
    | PWild 
    deriving (Show)

type UntypedTopLvl = TopLvl ()
type UntypedDecl = Decl ()
type UntypedStmt = Stmt ()
type UntypedExpr = Expr ()
