{-# LANGUAGE TupleSections #-}

module Desugar where

import qualified Data.Text as T
import qualified Data.Map as M
import Data.Maybe
import Data.Either.Combinators

import Control.Monad.State
import Control.Monad.Except

import Text.Megaparsec (sourcePosPretty)

import Analyzer.Substitution
import Analyzer.Unify
import Syntax
import SyntaxInfo
import Name
import Type

import Debug.Trace

data DesugarError = ManualInstantiation SyntaxInfo Name 

instance Show DesugarError where
    show (ManualInstantiation synInfo name) = "Could not deduce instantiation types for '" ++ show name ++ "', manual annotation required (" ++ sourcePosPretty (syntaxInfoSourcePos synInfo) ++ ")"

type Desugar a = ExceptT DesugarError (State DesugarState) a
data DesugarState = DesugarState
    { polyReqs :: M.Map (Name, Type) Int
    , polyDefs :: M.Map Name TypedTopLvl
    , polyFuncBodies :: M.Map Name TypedExpr
    , polyCount :: Int
    }
    
registerPolyDef :: Name -> TypedTopLvl -> Desugar ()
registerPolyDef name topLvl =
    modify (\state -> state { polyDefs = M.insert name topLvl (polyDefs state) })

addPolyReq :: Name -> Type -> Int -> Desugar ()
addPolyReq name typ id =
    modify (\state -> state { polyReqs = M.insert (name, typ) id (polyReqs state) })

desugar :: TypedProgram -> Either String TypedProgram
desugar prog = mapLeft show (evalState (runExceptT (desugarProgram prog)) initDesugarState)
    where
        initDesugarState = DesugarState {
            polyReqs = M.empty,
            polyDefs = M.empty,
            polyFuncBodies = M.empty,
            polyCount = 0
        }

desugarProgram :: TypedProgram -> Desugar TypedProgram
desugarProgram prog = do
    mapM_ (desugarTopLvl True) (concatMap modTopLvls prog)
    mods' <- traverse desugarModule prog

    -- lol just shove all the generated functions into the first module ig
    let (Module synInfo name path imports externs topLvls) = head mods'
    
    reqs <- gets polyReqs
    generatedReqs <- traverse genReq (M.toList reqs)
    
    let newFirstMod = Module synInfo name path imports externs (generatedReqs ++ topLvls)
    return (newFirstMod : tail mods')
    
genReq :: ((Name, Type), Int) -> Desugar TypedTopLvl
genReq ((name, typ), id) = do
    def <- gets ((M.! name) . polyDefs)
    let (synInfo, ftyp, isPub, isOper, fname, params, retAnnot, body) = getStuffFromFunc def
    case runSolve [Constraint synInfo typ ftyp] of
        Left err -> error ("[This shouldn't happen, please report] " ++ show err) -- this shouldnt happen
        Right subst -> do
            let newName = handlePolyName fname id
            body' <- gets ((M.! name) . polyFuncBodies)
            return (TLFunc synInfo (apply subst ftyp) isPub isOper newName params retAnnot body')
    where
        getStuffFromFunc (TLFunc synInfo ftyp isPub isOper name params retAnnot body) = (synInfo, ftyp, isPub, isOper, name, params, retAnnot, body)
        getStuffFromFunc _ = undefined

desugarModule :: TypedModule -> Desugar TypedModule
desugarModule (Module synInfo name path imports externs topLvls) = do
    newTopLvls <- catMaybes <$> traverse (desugarTopLvl False) topLvls
    return (Module synInfo name path imports externs newTopLvls)

desugarTopLvl :: Bool -> TypedTopLvl -> Desugar (Maybe TypedTopLvl)
desugarTopLvl firstPass tl@(TLFunc synInfo typ isPub isOper name params retAnnot body)
    | not (null (ftv typ)) = do
        when firstPass (registerPolyDef name tl)
        Nothing <$ unless firstPass (do
            body' <- desugarExpr body
            state <- get
            put (state { polyFuncBodies = M.insert name body' (polyFuncBodies state) }))
    | not firstPass = do
        body' <- desugarExpr body
        (return . Just) (TLFunc synInfo typ isPub isOper name params retAnnot body')
    | otherwise = return Nothing
desugarTopLvl _ other = return (Just other)

desugarDecl :: TypedDecl -> Desugar TypedDecl
desugarDecl (DStmt stmt) = DStmt <$> desugarStmt stmt
desugarDecl (DVar synInfo mut name annot expr) =
    DVar synInfo mut name annot <$> desugarExpr expr

desugarStmt :: TypedStmt -> Desugar TypedStmt
desugarStmt (SExpr expr) = SExpr <$> desugarExpr expr
desugarStmt (SRet expr) = SRet <$> desugarExpr expr
desugarStmt (SWhile synInfo cond body) =
    SWhile synInfo <$> desugarExpr cond <*> desugarExpr body

desugarExpr :: TypedExpr -> Desugar TypedExpr
desugarExpr (ECall synInfo typ (EVar vSynInfo vTyp name) args) = do
    args' <- traverse desugarExpr args
    name' <- handlePolyReq name (TArrow (map exprType args') typ)
    return (ECall synInfo typ (EVar vSynInfo vTyp name') args')
desugarExpr (EBinOp synInfo typ operName lhs rhs) = do
    polys <- gets polyDefs
    lhs' <- desugarExpr lhs
    rhs' <- desugarExpr rhs
    operName' <- handlePolyReq operName (TArrow [exprType lhs', exprType rhs'] typ)
    return (EBinOp synInfo typ operName' lhs' rhs')
desugarExpr (EUnaOp synInfo typ operName expr) = do
    expr' <- desugarExpr expr
    operName' <- handlePolyReq operName (TArrow [exprType expr'] typ)
    return (EUnaOp synInfo typ operName' expr')
desugarExpr e@(EVar synInfo typ name) = do
    polys <- gets polyDefs
    case M.lookup name polys of
        Just _ -> throwError (ManualInstantiation synInfo name)
        Nothing -> return e
desugarExpr (ECall synInfo typ expr args) =
    ECall synInfo typ <$> desugarExpr expr <*> traverse desugarExpr args
desugarExpr e@ELit {} = return e
desugarExpr (EAssign synInfo typ lhs rhs) =
    EAssign synInfo typ <$> desugarExpr lhs <*> desugarExpr rhs
desugarExpr (EBlock synInfo typ decls expr) = do
    EBlock synInfo typ <$> traverse desugarDecl decls <*> desugarExpr expr
desugarExpr (ECast synInfo targType expr) =
    ECast synInfo targType <$> desugarExpr expr
desugarExpr EClosure {} = error "Closures not supported yet"
desugarExpr (EIf synInfo typ cond a b) =
    EIf synInfo typ <$> desugarExpr cond <*> desugarExpr a <*> desugarExpr b
desugarExpr (EMatch synInfo typ mexpr branches) = do
    EMatch synInfo typ <$> desugarExpr mexpr <*> traverse desugarBranch branches
    where
        desugarBranch (pat, expr) = (pat, ) <$> desugarExpr expr
desugarExpr e@ERecordEmpty {} = return e
desugarExpr (ERecordExtend synInfo typ expr label rest) =
    ERecordExtend synInfo typ <$> desugarExpr expr <*> return label <*> desugarExpr rest
desugarExpr (ERecordRestrict synInfo typ expr label) =
    ERecordRestrict synInfo typ <$> desugarExpr expr <*> return label
desugarExpr (ERecordSelect synInfo typ expr label) =
    ERecordSelect synInfo typ <$> desugarExpr expr <*> return label
desugarExpr (EVariant synInfo typ enumName variant exprs) =
    EVariant synInfo typ enumName variant <$> traverse desugarExpr exprs
    
handlePolyReq :: Name -> Type -> Desugar Name
handlePolyReq name typ = do
    polys <- gets polyDefs
    reqs <- gets polyReqs
    maybeId <-
        if M.member name polys then
            case M.lookup (name, typ) reqs of
                Just id -> return (Just id)
                Nothing -> do
                    count <- gets polyCount
                    addPolyReq name typ count
                    state <- get
                    put (state { polyCount = count + 1 })
                    (return . Just) count
        else return Nothing
    case maybeId of
        Just id -> return (handlePolyName name id)
        Nothing -> return name

handlePolyName :: Name -> Int -> Name
handlePolyName name id =
    case name of
        Unqualified unqual -> Unqualified (T.concat [unqual, T.pack (show id)])
        Qualified quals n -> Qualified quals (T.concat [n, T.pack (show id)])   