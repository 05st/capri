{-# Language LambdaCase #-}
{-# Language DeriveFunctor #-}
{-# Language DeriveDataTypeable #-}

module Syntax where

import Data.Text (Text)
import Data.Data

import SyntaxInfo
import Type
import Name
import LabelMap

type Program a = [Module a]

type Import = (Bool, [Text])
data Module a = Module
    { modSynInfo :: SyntaxInfo
    , modName :: Text
    , modPath :: [Text]
    , modImports :: [Import]
    , modTopLvls :: [TopLvl a]
    } deriving (Show, Functor)

data TopLvl a
    = TLFunc SyntaxInfo Bool Bool Name Params TypeAnnot (Expr a)
    | TLType SyntaxInfo Bool Name [TVar] Type
    | TLExtern Text [Type] Type
    deriving (Show, Functor)
    
data Decl a
    = DStmt (Stmt a)
    | DVar SyntaxInfo Bool Name TypeAnnot (Expr a)
    deriving (Show, Functor, Data)

data Stmt a
    = SExpr (Expr a)
    | SRet (Expr a)
    | SWhile SyntaxInfo (Expr a) (Expr a)
    deriving (Show, Functor, Data)

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
    | ERecordEmpty SyntaxInfo a
    | ERecordSelect SyntaxInfo a (Expr a) Text
    | ERecordRestrict SyntaxInfo a (Expr a) Text
    | ERecordExtend SyntaxInfo a (Expr a) Text (Expr a)
    -- | ERecordExtend SyntaxInfo a (LabelMap (Expr a)) (Expr a)
    | EVariant SyntaxInfo a (Expr a) Text
    deriving (Show, Functor, Data)

type TypeAnnot = Maybe Type
type Params = [(Text, TypeAnnot)]

data Lit
    = LInt Integer
    | LFloat Double
    | LString Text
    | LChar Char
    | LBool Bool
    | LUnit
    deriving (Show, Data)

data Pattern
    = PVariant Text Pattern
    | PLit Lit
    | PVar Text
    | PWild 
    deriving (Show, Data)

type UntypedProgram = Program ()
type UntypedModule = Module ()
type UntypedTopLvl = TopLvl ()
type UntypedDecl = Decl ()
type UntypedStmt = Stmt ()
type UntypedExpr = Expr ()

type TypedProgram = Program Type
type TypedModule = Module Type
type TypedTopLvl = TopLvl Type
type TypedDecl = Decl Type
type TypedStmt = Stmt Type
type TypedExpr = Expr Type

getModFullName :: Module a -> [Text]
getModFullName mod = modPath mod ++ [modName mod]

topLvlToName :: TopLvl a -> Name
topLvlToName (TLFunc _ _ _ name _ _ _) = name
topLvlToName (TLType _ _ name _ _) = name
topLvlToName (TLExtern name _ _) = Unqualified name

isTopLvlPub :: TopLvl a -> Bool
isTopLvlPub (TLFunc _ isPub _ _ _ _ _) = isPub
isTopLvlPub (TLType _ isPub _ _ _) = isPub
isTopLvlPub TLExtern {} = False

exprType :: TypedExpr -> Type
exprType = \case
    ELit _ t _ -> t
    EVar _ t _ _ -> t
    EAssign _ t _ _ -> t
    EBlock _ t _ _ -> t
    EIf _ t _ _ _ -> t
    EMatch _ t _ _ -> t
    EBinOp _ t _ _ _ -> t
    EUnaOp _ t _ _ -> t
    EClosure _ t _ _ _ _ -> t
    ECall _ t _ _ -> t
    ECast _ t _ -> t
    ERecordEmpty _ t -> t
    ERecordSelect _ t _ _ -> t
    ERecordRestrict _ t _ _ -> t
    ERecordExtend _ t _ _ _ -> t
    EVariant _ t _ _ -> t