{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

module Monomorphize where

import qualified Data.Map as M
import qualified Data.Text as T
import Data.Maybe
import Data.List
import Data.Either.Combinators

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
    }

registerPoly :: Name -> TypedTopLvl -> Mono ()
registerPoly name topLvl =
    modify (\state -> state { polys = M.insert name topLvl (polys state) })

monomorphize :: TypedProgram -> Either String TypedProgram
monomorphize prog = mapLeft show (evalState (runExceptT (monomorphizeProgram prog)) initMonoState)
    where
        initMonoState = MonoState {
            polys = M.empty,
            requests = []
        }

monomorphizeProgram :: TypedProgram -> Mono TypedProgram
monomorphizeProgram prog = do
    mapM_ (monomorphizeTopLvl True) (concatMap modTopLvls prog)
    mods' <- traverse monomorphizeModule prog

    generatedTopLvls <- genRequests 0

    -- Put the generated top levels into a new module. SyntaxInfo doesn't matter so just copy one.
    let polyMod = Module (modSynInfo (head mods')) (T.pack "_polys") [] [] [] generatedTopLvls
    return (polyMod : mods')

genRequests :: Int -> Mono [TypedTopLvl]
genRequests idx = do
    reqs <- gets requests
    if idx >= length reqs
        then return []
        else do
            let (reqName, reqType) = reqs !! idx
            def <- gets ((M.! reqName) . polys)
            
            let (synInfo, ftyp, isPub, isOper, fname, params, retAnnot, body) = getStuffFromFunc def
            case runSolve [Constraint synInfo reqType ftyp] of
                Left err -> error ("[Shouldn't occur, please report] " ++ show err)
                Right subst -> do
                    let newName = handlePolyName fname idx
                    newBody <- monomorphizeExpr (apply subst <$> body)

                    let newFunc = TLFunc synInfo (apply subst ftyp) isPub isOper newName params retAnnot newBody
                    (newFunc :) <$> genRequests (idx + 1)
    where
        getStuffFromFunc (TLFunc synInfo ftyp isPub isOper name params retAnnot body) = (synInfo, ftyp, isPub, isOper, name, params, retAnnot, body)
        getStuffFromFunc _ = undefined

monomorphizeModule :: TypedModule -> Mono TypedModule
monomorphizeModule (Module synInfo name path imports externs topLvls) = do
    newTopLvls <- catMaybes <$> traverse (monomorphizeTopLvl False) topLvls
    return (Module synInfo name path imports externs newTopLvls)

monomorphizeTopLvl :: Bool -> TypedTopLvl -> Mono (Maybe TypedTopLvl)
monomorphizeTopLvl firstPass topLvl@(TLFunc synInfo typ isPub isOper name params retAnnot body)
    | not (null (ftv typ)) = Nothing <$ when firstPass (registerPoly name topLvl)
    | not firstPass = do
        body' <- monomorphizeExpr body
        (return . Just) (TLFunc synInfo typ isPub isOper name params retAnnot body')
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
    EVariant synInfo typ enumName variant exprs ->
        EVariant synInfo typ enumName variant <$> traverse monomorphizeExpr exprs

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
