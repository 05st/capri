{-# Language OverloadedStrings #-}
{-# Language LambdaCase #-}
{-# Language TupleSections #-}

module Analyzer.Infer where

import Data.Maybe
import Data.Text (Text, pack)
import qualified Data.Map as M
import qualified Data.Set as S
import Control.Monad.State
import Control.Monad.Except

import Text.Megaparsec.Pos (SourcePos)

import Syntax
import SyntaxInfo
import Type
import Name

import Analyzer.AnalyzerError
import Analyzer.Substitution

import Debug.Trace

type Env = M.Map Name PolyType -- We don't need to keep track if they are public, the resolver took care of that
type Infer = ExceptT AnalyzerError (State InferState)
data InferState = InferState
    { environment :: Env
    , isMutEnv :: M.Map Name Bool 
    , typeAliasEnv :: M.Map Name Type
    , preTopLvlEnv :: M.Map Name TVar
    , freshCount :: Int
    , constraints :: [Constraint]
    } deriving (Show)

data Constraint
    = Constraint SourcePos Type Type
    deriving (Show)

inferProgram :: UntypedProgram -> Either AnalyzerError TypedProgram
inferProgram prog = evalState (runExceptT (preInference prog *> traverse inferModule prog)) initInferState
    where
        initInferState = InferState { environment = M.empty, isMutEnv = M.empty, typeAliasEnv = M.empty, preTopLvlEnv = M.empty, freshCount = 0, constraints = [] }

preInference :: UntypedProgram -> Infer ()
preInference prog = mapM_ initializeTopLvl (concatMap modTopLvls prog)
    where
        initializeTopLvl (TLFunc _ _ _ name _ _ _) = do
            env <- gets preTopLvlEnv
            state <- get
            typeVar <- fresh
            let TVar tv = typeVar
            put (state { preTopLvlEnv = M.insert name tv env })
        initializeTopLvl (TLType _ isPub name tvars typ) = do
            env <- gets typeAliasEnv
            state <- get
            put (state { typeAliasEnv = M.insert name typ env })
        initializeTopLvl TLExtern {} = undefined

inferModule :: UntypedModule -> Infer TypedModule
inferModule mod = do
    env <- gets environment
    trace (show env) $ return ()
    undefined

inferDecl :: UntypedDecl -> Infer TypedDecl
inferDecl = undefined

inferExpr :: UntypedExpr -> Infer TypedExpr
inferExpr = \case
    ELit info _ lit -> return (ELit info (inferLit lit) lit)

    EVar info _ _ name -> do
        typ <- lookupType info name
        return (EVar info typ [] name)

    EAssign info _ l r -> do
        l' <- inferExpr l
        r' <- inferExpr r
        let ltype = exprType l'
        let rtype = exprType r'
        constrain (Constraint (syntaxInfoSourcePos info) ltype rtype)
        let expr' = EAssign info ltype l' r'
        case l' of
            EVar vinfo _ _ name -> do
                mut <- lookupMut vinfo name
                unless mut (throwError (GenericAnalyzerError (syntaxInfoSourcePos info) ("Cannot assign to immutable variable '" ++ show name ++ "'")))
                return expr'
            _ -> throwError (GenericAnalyzerError (syntaxInfoSourcePos info) "Cannot assign to non-lvalue")

    EBlock info _ decls expr -> do
        (decls', expr') <- scoped id (do
            ds <- traverse inferDecl decls
            ex <- inferExpr expr
            return (ds, ex))
        return (EBlock info (exprType expr') decls' expr')

    EIf info _ cond texpr fexpr -> do
        cond' <- inferExpr cond
        texpr' <- inferExpr texpr
        fexpr' <- inferExpr fexpr
        let ctype = exprType cond'
        let ttype = exprType texpr'
        let ftype = exprType fexpr'
        constrain (Constraint (syntaxInfoSourcePos info) ctype TBool)
        constrain (Constraint (syntaxInfoSourcePos info) ttype ftype)
        return (EIf info ttype cond' texpr' fexpr')
    
    EMatch info _ mexpr branches -> do
        mexpr' <- inferExpr mexpr
        let mtype = exprType mexpr'
        (branches', btypes) <- unzip <$> traverse (inferBranch info mtype) branches
        case btypes of
            [] -> throwError (GenericAnalyzerError (syntaxInfoSourcePos info) "Empty match expression")
            (btype : rest) -> (EMatch info btype mexpr' branches') <$ mapM_ (constrain . Constraint (syntaxInfoSourcePos info) btype) rest

inferBranch :: SyntaxInfo -> Type -> (Pattern, UntypedExpr) -> Infer ((Pattern, TypedExpr), Type)
inferBranch info mt (pat, expr) = do
    let pos = syntaxInfoSourcePos info
    (pt, vars) <- inferPattern pos pat
    constrain (Constraint pos pt mt)
    expr' <- scoped (M.fromList vars `M.union`) (inferExpr expr)
    return ((pat, expr'), exprType expr')

inferLit :: Lit -> Type
inferLit = \case
    LInt _ -> TInt64
    LFloat _ -> TFloat64
    LString _ -> TString
    LChar _ -> TChar
    LBool _ -> TBool
    LUnit -> TUnit

constrain :: Constraint -> Infer ()
constrain const = do
    state <- get
    put (state { constraints = const : constraints state })

fresh :: Infer Type
fresh = do
    state <- get
    count <- gets freshCount
    put (state { freshCount = count + 1 })
    return . TVar . TV . pack . ('_':) $ varNames !! count
    where
        varNames = [1..] >>= flip replicateM ['a'..'z']

generalize :: Env -> Type -> PolyType
generalize env typ = Forall (S.toList vs) typ
    where
        vs = tvs typ `S.difference` tvs (M.elems env)

instantiate :: PolyType -> Infer Type
instantiate (Forall vs typ) = do
    nvs <- traverse (const fresh) vs
    let sub = M.fromList (zip vs nvs)
    return (apply sub typ)

lookupVar :: SyntaxInfo -> Name -> Infer (Type, Bool)
lookupVar info name = do
    env <- gets environment
    mutEnv <- gets isMutEnv
    let mut = fromMaybe False (M.lookup name mutEnv)
    case M.lookup name env of
        Just typ -> (,mut) <$> instantiate typ
        Nothing -> do
            preEnv <- gets preTopLvlEnv
            case M.lookup name preEnv of
                Just tvar -> return (TVar tvar, False)
                Nothing -> throwError (UndefinedError (syntaxInfoSourcePos info) (show name))


lookupType :: SyntaxInfo -> Name -> Infer Type
lookupType info name = fst <$> lookupVar info name
            
lookupMut :: SyntaxInfo -> Name -> Infer Bool
lookupMut info name = snd <$> lookupVar info name

scoped :: (Env -> Env) -> Infer a -> Infer a
scoped fn m = do
    state <- get
    let env = environment state
    put (state { environment = fn env })
    res <- m
    state' <- get
    put (state' { environment = env })
    return res