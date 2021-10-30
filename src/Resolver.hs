{-# Language LambdaCase #-}

module Resolver where

import Data.Text (Text)
import Control.Monad.Reader

import Syntax
import Name

type Resolve = Reader [Text]

resolve :: UntypedProgram -> UntypedProgram
resolve prog = runReader (resolveProgram prog) []

resolveProgram :: UntypedProgram -> Resolve UntypedProgram
resolveProgram = traverse resolveModule

resolveModule :: UntypedModule -> Resolve UntypedModule
resolveModule (Module name imports topLvls pubs) = do
    topLvls' <- local (const name) (traverse resolveTopLvl topLvls)
    return (Module name imports topLvls' pubs)

resolveTopLvl :: UntypedTopLvl -> Resolve UntypedTopLvl
resolveTopLvl = \case
    TLFunc t name params tann body -> do
        mod <- ask
        body' <- resolveExpr body
        return (TLFunc t (Qualified (mod ++ [extractName name])) params tann body')
    TLOper t opdef name params tann body -> do
        mod <- ask
        body' <- resolveExpr body
        return (TLOper t opdef (Qualified (mod ++ [extractName name])) params tann body')
    TLType name tvars cons -> do
        mod <- ask
        let (conNames, conTypes) = unzip cons
        let name' = Qualified (mod ++ [extractName name])
        let conNames' = map (\n -> Qualified (mod ++ [extractName n])) conNames
        return (TLType name' tvars (zip conNames' conTypes))
    other -> return other 

resolveDecl :: UntypedDecl -> Resolve UntypedDecl
resolveDecl = \case
    DVar t mut name tann expr -> do
        expr' <- resolveExpr expr
        return (DVar t mut name tann expr')
    DStmt s -> DStmt <$> resolveStmt s

resolveStmt :: UntypedStmt -> Resolve UntypedStmt
resolveStmt = \case
    SExpr e -> SExpr <$> resolveExpr e
    SRet e -> SRet <$> resolveExpr e
    SWhile cond e -> do
        cond' <- resolveExpr cond
        e' <- resolveExpr e
        return (SWhile cond' e')

resolveExpr :: UntypedExpr -> Resolve UntypedExpr
resolveExpr = \case
    l@(ELit _ _) -> return l
    EVar t name -> do
        mod <- ask
        return (EVar t (Qualified (mod ++ [extractName name])))
    EBinOp t name a b -> do
        mod <- ask
        return (EBinOp t (Qualified (mod ++ [extractName name])) a b)
    EUnaOp t name a -> do   
        mod <- ask
        return (EUnaOp t (Qualified (mod ++ [extractName name])) a)
    EBlock t decls res -> do
        decls' <- traverse resolveDecl decls
        res' <- resolveExpr res
        return (EBlock t decls' res')
    EAssign t l r -> do
        l' <- resolveExpr l
        r' <- resolveExpr r
        return (EAssign t l' r')
    EIf t cond a b -> do
        cond' <- resolveExpr cond
        a' <- resolveExpr a
        b' <- resolveExpr b
        return (EIf t cond' a' b')
    EMatch t mexpr branches -> do
        let (bpats, bexprs) = unzip branches
        bpats' <- traverse resolvePattern bpats
        bexprs' <- traverse resolveExpr bexprs
        mexpr' <- resolveExpr mexpr
        return (EMatch t mexpr' (zip bpats' bexprs'))
    EClosure t cvars params tann body -> do 
        body' <- resolveExpr body
        return (EClosure t cvars params tann body')
    ECall t cexpr args -> do
        args' <- traverse resolveExpr args
        cexpr' <- resolveExpr cexpr
        return (ECall t cexpr' args')
    ECast t targ arg -> ECast t targ <$> resolveExpr arg
    EDeref t expr -> EDeref t <$> resolveExpr expr
    ERef t expr -> ERef t <$> resolveExpr expr
    ESizeof t (Right expr) -> ESizeof t . Right <$> resolveExpr expr
    e@(ESizeof _ _) -> return e
    EArray t exprs -> EArray t <$> traverse resolveExpr exprs
    EIndex t expr idx -> flip (EIndex t) idx <$> resolveExpr expr

resolvePattern :: Pattern -> Resolve Pattern
resolvePattern = \case
    PCon name binds -> do
        mod <- ask
        return (PCon (Qualified (mod ++ [extractName name])) binds)
    other -> return other
