{-# Language LambdaCase #-}

module Resolver where

import Data.Text (Text)
import Control.Monad.Reader

import Syntax
import Name
import Type
import Lexer

import Debug.Trace

type Resolve = Reader [Text]

resolve :: UntypedProgram -> UntypedProgram
resolve prog = runReader (resolveProgram prog) []

resolveProgram :: UntypedProgram -> Resolve UntypedProgram
resolveProgram = traverse resolveModule

resolveModule :: UntypedModule -> Resolve UntypedModule
resolveModule (Module pos name imports topLvls pubs) = do
    topLvls' <- local (const name) (traverse resolveTopLvl topLvls)
    return (Module pos name imports topLvls' pubs)

resolveTopLvl :: UntypedTopLvl -> Resolve UntypedTopLvl
resolveTopLvl = \case
    TLFunc t pos name params tann body -> do
        let (pnames, ptypes) = unzip params
        name' <- fixName name
        body' <- resolveExpr body
        ptypes' <- traverse resolveTypeAnnot ptypes
        tann' <- resolveTypeAnnot tann
        return (TLFunc t pos name' (zip pnames ptypes') tann' body')
    TLOper t pos name params tann body -> do
        let (pnames, ptypes) = unzip params
        name' <- fixName name
        body' <- resolveExpr body
        ptypes' <- traverse resolveTypeAnnot ptypes
        tann' <- resolveTypeAnnot tann
        return (TLOper t pos name' (zip pnames ptypes') tann' body')
    TLType pos name tvars cons -> do
        name' <- fixName name
        let (conNames, conTypes) = unzip cons
        conNames' <- traverse fixName conNames
        conTypes' <- traverse (traverse resolveType) conTypes
        return (TLType pos name' tvars (zip conNames' conTypes'))
    TLStruct pos name tvars fields -> do
        name' <- fixName name
        return (TLStruct pos name' tvars fields)
    a -> return a

resolveDecl :: UntypedDecl -> Resolve UntypedDecl
resolveDecl = \case
    DVar t pos mut name tann expr -> do
        expr' <- resolveExpr expr
        tann' <- resolveTypeAnnot tann
        return (DVar t pos mut name tann' expr')
    DStmt s -> DStmt <$> resolveStmt s

resolveStmt :: UntypedStmt -> Resolve UntypedStmt
resolveStmt = \case
    SExpr e -> SExpr <$> resolveExpr e
    SRet e -> SRet <$> resolveExpr e
    SWhile pos cond e -> do
        cond' <- resolveExpr cond
        e' <- resolveExpr e
        return (SWhile pos cond' e')

resolveExpr :: UntypedExpr -> Resolve UntypedExpr
resolveExpr = \case
    EVar t p gs name -> do
        name' <- fixName name
        return (EVar t p gs name')
    EBinOp t p name a b -> do
        name' <- fixName name
        a' <- resolveExpr a
        b' <- resolveExpr b
        return (EBinOp t p name' a' b')
    EUnaOp t p name a -> do   
        name' <- fixName name
        a' <- resolveExpr a
        return (EUnaOp t p name' a')
    EBlock t p decls res -> do
        decls' <- traverse resolveDecl decls
        res' <- resolveExpr res
        return (EBlock t p decls' res')
    EAssign t p l r -> do
        l' <- resolveExpr l
        r' <- resolveExpr r
        return (EAssign t p l' r')
    EIf t p cond a b -> do
        cond' <- resolveExpr cond
        a' <- resolveExpr a
        b' <- resolveExpr b
        return (EIf t p cond' a' b')
    EMatch t p mexpr branches -> do
        let (bpats, bexprs) = unzip branches
        bpats' <- traverse resolvePattern bpats
        bexprs' <- traverse resolveExpr bexprs
        mexpr' <- resolveExpr mexpr
        return (EMatch t p mexpr' (zip bpats' bexprs'))
    EClosure t p cvars params tann body -> do 
        body' <- resolveExpr body
        return (EClosure t p cvars params tann body')
    ECall t p cexpr args -> do
        args' <- traverse resolveExpr args
        cexpr' <- resolveExpr cexpr
        return (ECall t p cexpr' args')
    ECast t p targ arg -> ECast t p targ <$> resolveExpr arg
    EDeref t p expr -> EDeref t p <$> resolveExpr expr
    ERef t p expr -> ERef t p <$> resolveExpr expr
    ESizeof t p (Right expr) -> ESizeof t p . Right <$> resolveExpr expr
    EArray t p exprs -> EArray t p <$> traverse resolveExpr exprs
    EIndex t p expr idx -> flip (EIndex t p) idx <$> resolveExpr expr
    EStruct t p structName fields -> flip (EStruct t p) fields <$> fixName structName
    EAccess t p expr label -> flip (EAccess t p) label <$> resolveExpr expr
    a -> return a

resolvePattern :: Pattern -> Resolve Pattern
resolvePattern = \case
    PCon name binds -> do
        name' <- fixName name
        return (PCon name' binds)
    a -> return a

resolveType :: Type -> Resolve Type
resolveType = \case
    TCon name types | extractName name `notElem` reservedNames -> flip TCon types <$> fixName name
    TPtr t -> TPtr <$> resolveType t
    TArray t -> TArray <$> resolveType t
    TFunc ptypes rtype -> do
        ptypes' <- traverse resolveType ptypes
        TFunc ptypes' <$> resolveType rtype
    a -> return a

resolveTypeAnnot :: Maybe Type -> Resolve (Maybe Type)
resolveTypeAnnot = \case
    Just t -> Just <$> resolveType t
    Nothing -> return Nothing

fixName :: Name -> Resolve Name
fixName name = do
    mod <- ask
    return (Qualified (mod ++ [extractName name]))
