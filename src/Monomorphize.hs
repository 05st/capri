{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

module Monomorphize where

import qualified Data.Map as M
import qualified Data.Text as T
import Data.Maybe
import Data.List
import Data.Either.Combinators
import Data.Generics.Uniplate.Data

import Control.Monad.State
import Control.Monad.Except

import Text.Megaparsec (sourcePosPretty)

import Analyzer.Substitution
import Analyzer.Unify
import Syntax
import SyntaxInfo
import Type
import Name

import Debug.Trace

data MonoError = ManualInstantiation SyntaxInfo Name 
instance Show MonoError where
    show (ManualInstantiation synInfo name)
        = "Could not deduce instantiation types for '" ++ show name ++ "', manual annotation required (" ++ sourcePosPretty (syntaxInfoSourcePos synInfo) ++ ")"

type Mono a = ExceptT MonoError (State MonoState) a
data MonoState = MonoState
    { polys :: M.Map Name TypedTopLvl
    , requests :: [(Name, Type)]
    , enumSubst :: M.Map Type Type
    }
    
applyEnumSubst :: M.Map Type Type -> Type -> Type
applyEnumSubst subst = transform go
    where
        go t@TApp {} = fromMaybe t (M.lookup t subst) 
        go other = other

registerPoly :: TypedTopLvl -> Mono ()
registerPoly topLvl =
    modify (\state -> state { polys = M.insert (topLvlToName topLvl) topLvl (polys state) })

monomorphize :: TypedProgram -> Either String TypedProgram
monomorphize prog = mapLeft show (evalState (runExceptT (monomorphizeProgram prog)) initMonoState)
    where
        initMonoState = MonoState {
            polys = M.empty,
            requests = [],
            enumSubst = M.empty
        }

monomorphizeProgram :: TypedProgram -> Mono TypedProgram
monomorphizeProgram prog = do
    mapM_ (monomorphizeTopLvl True) (concatMap modTopLvls prog)
    mods' <- traverse monomorphizeModule prog

    generatedTopLvls <- genRequests 0
    subst <- gets enumSubst
    let newTopLvls = fmap (applyEnumSubst subst) <$> generatedTopLvls

    -- Put the generated top levels into a new module. SyntaxInfo doesn't matter so just copy one.
    let polyMod = Module (modSynInfo (head mods')) (T.pack "_polys") [] [] [] newTopLvls
    return (polyMod : mods')

genRequests :: Int -> Mono [TypedTopLvl]
genRequests idx = do
    reqs <- gets requests
    if idx >= length reqs
        then return []
        else do
            let (reqName, reqType) = reqs !! idx
            def <- gets ((M.! reqName) . polys)
            
            newTopLvl <- genReq reqType def
            (newTopLvl :) <$> genRequests (idx + 1)
    where
        genReq reqType (TLFunc synInfo ftyp isPub isOper fname params retAnnot body) = do
            case runSolve [Constraint synInfo reqType ftyp] of
                Left err -> undefined -- Should never occur
                Right subst -> do
                    let newName = handlePolyName fname idx
                    newBody <- monomorphizeExpr (apply subst <$> body)
                    return (TLFunc synInfo (apply subst ftyp) isPub isOper newName params retAnnot newBody)
        genReq reqType (TLEnum synInfo isPub enumName tvars variants) = do
            case runSolve [Constraint synInfo reqType (TApp (TConst enumName) (map TVar tvars))] of
                Left err -> undefined -- Should never occur
                Right subst -> do
                    let (variantNames, variantTypes) = unzip variants
                    let newVariantTypes = apply subst variantTypes
                    let newName = handlePolyName enumName idx
                    
                    state <- get
                    put (state { enumSubst = M.insert reqType (TConst newName) (enumSubst state) })

                    return (TLEnum synInfo isPub newName [] (zip variantNames newVariantTypes))
        genReq _ _ = undefined

monomorphizeModule :: TypedModule -> Mono TypedModule
monomorphizeModule (Module synInfo name path imports externs topLvls) = do
    newTopLvls <- catMaybes <$> traverse (monomorphizeTopLvl False) topLvls
    return (Module synInfo name path imports externs newTopLvls)

monomorphizeTopLvl :: Bool -> TypedTopLvl -> Mono (Maybe TypedTopLvl)
monomorphizeTopLvl firstPass topLvl@(TLFunc synInfo typ isPub isOper name params retAnnot body)
    | not (null (ftv typ)) = Nothing <$ when firstPass (registerPoly topLvl)
    | not firstPass = Just . TLFunc synInfo typ isPub isOper name params retAnnot <$> monomorphizeExpr body
    | otherwise = return Nothing
monomorphizeTopLvl firstPass topLvl@(TLEnum _ _ _ tvars _ )
    | not (null tvars) = Nothing <$ registerPoly topLvl
    | not firstPass = return (Just topLvl)
    | otherwise = return Nothing
monomorphizeTopLvl _ other = return (Just other)

monomorphizeDecl :: TypedDecl -> Mono TypedDecl
monomorphizeDecl (DStmt stmt) = DStmt <$> monomorphizeStmt stmt
monomorphizeDecl (DVar synInfo mut name annot expr) =
    DVar synInfo mut name annot <$> monomorphizeExpr expr

monomorphizeStmt :: TypedStmt -> Mono TypedStmt
monomorphizeStmt (SExpr expr) = SExpr <$> monomorphizeExpr expr
monomorphizeStmt (SRet expr) = SRet <$> monomorphizeExpr expr
monomorphizeStmt (SWhile synInfo cond body) =
    SWhile synInfo <$> monomorphizeExpr cond <*> monomorphizeExpr body

monomorphizeExpr :: TypedExpr -> Mono TypedExpr
monomorphizeExpr = \case
    ECall synInfo typ (EVar varSynInfo varTyp name) args -> do
        args' <- traverse monomorphizeExpr args
        name' <- handlePolyReq name (TArrow (map exprType args') typ)
        return (ECall synInfo typ (EVar varSynInfo varTyp name') args')
    EBinOp synInfo typ operName lhs rhs -> do
        lhs' <- monomorphizeExpr lhs
        rhs' <- monomorphizeExpr rhs
        operName' <- handlePolyReq operName (TArrow [exprType lhs', exprType rhs'] typ)
        return (EBinOp synInfo typ operName' lhs' rhs')
    EUnaOp synInfo typ operName expr -> do
        expr' <- monomorphizeExpr expr
        operName' <- handlePolyReq operName (TArrow [exprType expr'] typ)
        return (EUnaOp synInfo typ operName' expr')

    expr@(EVar synInfo typ name) -> do
        isPoly <- gets (M.lookup name . polys)
        case isPoly of
            Just _ -> throwError (ManualInstantiation synInfo name)
            Nothing -> return expr

    expr@ELit {} -> return expr
    ECall synInfo typ expr args ->
        ECall synInfo typ <$> monomorphizeExpr expr <*> traverse monomorphizeExpr args
    EAssign synInfo typ lhs rhs ->
        EAssign synInfo typ <$> monomorphizeExpr lhs <*> monomorphizeExpr rhs
    EBlock synInfo typ decls expr ->
        EBlock synInfo typ <$> traverse monomorphizeDecl decls <*> monomorphizeExpr expr
    ECast synInfo targType expr ->
        ECast synInfo targType <$> monomorphizeExpr expr
    EClosure {} -> error "Closures not supported yet"
    EIf synInfo typ cond a b ->
        EIf synInfo typ <$> monomorphizeExpr cond <*> monomorphizeExpr a <*> monomorphizeExpr b
    EMatch synInfo typ mexpr branches ->
        EMatch synInfo typ <$> monomorphizeExpr mexpr <*> traverse monomorphizeBranch branches
    expr@ERecordEmpty {} -> return expr
    ERecordExtend synInfo typ expr label rest ->
        ERecordExtend synInfo typ <$> monomorphizeExpr expr <*> return label <*> monomorphizeExpr rest
    ERecordRestrict synInfo typ expr label ->
        ERecordRestrict synInfo typ <$> monomorphizeExpr expr <*> return label
    ERecordSelect synInfo typ expr label ->
        ERecordSelect synInfo typ <$> monomorphizeExpr expr <*> return label
    EVariant synInfo typ enumName variant exprs -> do
        enumName' <- handlePolyReq enumName typ
        EVariant synInfo typ enumName' variant <$> traverse monomorphizeExpr exprs

    where
        monomorphizeBranch (pat, expr) = (pat, ) <$> monomorphizeExpr expr

handlePolyReq :: Name -> Type -> Mono Name
handlePolyReq name typ = do
    ps <- gets polys
    reqs <- gets requests
    maybeId <-
        if M.member name ps then
            case elemIndex (name, typ) reqs of
                Just id -> return (Just id)
                Nothing -> do
                    state <- get
                    put (state { requests = requests state ++ [(name, typ)] })
                    (return . Just) (length (requests state))
        else return Nothing
    case maybeId of
        Just id -> return (handlePolyName name id)
        Nothing -> return name

handlePolyName :: Name -> Int -> Name
handlePolyName name id =
    case name of
        Unqualified unqual -> Unqualified (T.concat [unqual, T.pack (show id)])
        Qualified quals n -> Qualified quals (T.concat [n, T.pack (show id)])
