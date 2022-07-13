{-# Language OverloadedStrings #-}
{-# Language LambdaCase #-}
{-# Language TupleSections #-}

module Analyzer.Infer where

import Data.Maybe
import Data.Text (Text, pack, unpack)
import Data.Data
import Data.Generics.Uniplate.Data
import Data.Functor.Identity
import qualified Data.Map as M
import qualified Data.Set as S
import Control.Monad.State
import Control.Monad.Except
import Control.Monad.Reader

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
    , preTopLvlEnv :: M.Map Name TVar
    , freshCount :: Int
    , constraints :: [Constraint]
    } deriving (Show)

inferProgram :: UntypedProgram -> Either AnalyzerError TypedProgram
inferProgram prog =
    case runState (runExceptT (preInference prog *> traverse inferModule prog)) initInferState of
        (Left err, _) -> Left err
        (Right prog', state) -> do
            subst <- runSolve (constraints state)
            return $ fmap (fmap (apply subst)) prog'
    where
        initInferState = InferState { environment = M.empty, isMutEnv = M.empty, preTopLvlEnv = M.empty, freshCount = 0, constraints = [] }

preInference :: UntypedProgram -> Infer ()
preInference prog = mapM_ initializeTopLvl (concatMap modTopLvls prog)
    where
        initializeTopLvl (TLFunc _ _ _ _ name _ _ _) = do
            env <- gets preTopLvlEnv
            state <- get
            typeVar <- fresh
            let TVar tv = typeVar
            put (state { preTopLvlEnv = M.insert name tv env })
        initializeTopLvl TLType {} = return ()

inferModule :: UntypedModule -> Infer TypedModule
inferModule (Module info name path imports topLvls) = do
    env <- gets environment
    -- trace (show env) $ return ()
    topLvls' <- traverse inferTopLvl topLvls
    return (Module info name path imports topLvls')

inferTopLvl :: UntypedTopLvl -> Infer TypedTopLvl
inferTopLvl = \case
    TLFunc info _ isPub isOper name params annot body -> do
        alreadyDefined <- exists name
        when alreadyDefined (throwError (RedefinitionError (syntaxInfoSourcePos info) (show name)))
        (body', typ) <- inferFn (syntaxInfoSourcePos info) name params annot body
        return (TLFunc info typ isPub isOper name params annot body')
    TLType info isPub name typeParams mainTyp -> return (TLType info isPub name typeParams mainTyp)

inferFn :: SourcePos -> Name -> Params -> TypeAnnot -> UntypedExpr -> Infer (TypedExpr, Type)
inferFn pos name params rtann body = do
    let (pnames, panns) = unzip params
    ptypes <- traverse (const fresh) params
    sequence_ [when (isJust pann) (constrain $ Constraint pos ptype (fromJust pann)) | (ptype, pann) <- zip ptypes panns]

    let ptypesSchemes = map (Forall []) ptypes
    let nenv = M.fromList (zip (map Unqualified pnames) ptypesSchemes) -- Default mutability is false when variable exists in type env (maybe make explicit?)
    body' <- scoped (`M.union` nenv) (inferExpr body)
    let rtype = exprType body'
    consts <- gets constraints
    subst <- liftEither (runSolve consts)
    env <- gets environment
    let typ = apply subst (TArrow ptypes rtype)
        scheme = Forall [] typ -- TODO: generalize env typ for polymorphism

    let (TArrow ptypes' rtype') = typ
    when (isJust rtann) (constrain $ Constraint pos rtype' (fromJust rtann))
    sequence_ [when (isJust pann) (constrain $ Constraint pos ptype (fromJust pann)) | (ptype, pann) <- zip ptypes' panns]

    mapM_ (constrain . Constraint pos rtype') (searchReturns body')

    state <- get
    let tmpsEnv = preTopLvlEnv state
    constrain (Constraint pos typ (TVar (fromJust (M.lookup name tmpsEnv))))
    put (state { preTopLvlEnv = M.delete name tmpsEnv })

    state <- get
    put (state { environment = M.insert name scheme (environment state) })
    return (body', typ)

inferDecl :: UntypedDecl -> Infer TypedDecl
inferDecl = \case
    DVar info mut name annot expr -> do
        alreadyDefined <- exists name
        when alreadyDefined (throwError (RedefinitionError (syntaxInfoSourcePos info) (show name)))
        expr' <- inferExpr expr
        let etype = exprType expr'
        consts <- gets constraints
        subst <- liftEither (runSolve consts)
        env <- gets environment
        let typ = apply subst etype
            scheme = Forall [] typ -- TODO: generalize (maybe?)
        when (isJust annot) (constrain (Constraint (syntaxInfoSourcePos info) typ (fromJust annot)))
        state <- get
        put (state {
            environment = M.insert name scheme (environment state),
            isMutEnv = M.insert name mut (isMutEnv state)
        })
        return (DVar info mut name annot expr')
    DStmt s -> DStmt <$> inferStmt s

inferStmt :: UntypedStmt -> Infer TypedStmt
inferStmt = \case
    SRet expr -> SRet <$> inferExpr expr
    SWhile info cond body -> do
        cond' <- inferExpr cond
        let ctype = exprType cond'
        body' <- inferExpr body
        constrain (Constraint (syntaxInfoSourcePos info) ctype TBool)
        return (SWhile info cond' body')
    SExpr expr -> SExpr <$> inferExpr expr

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
        case l of
            EVar vinfo _ _ name -> do
                mut <- lookupMut vinfo name
                unless mut (throwError (GenericAnalyzerError (syntaxInfoSourcePos info) ("Cannot assign to immutable variable '" ++ show name ++ "'")))
                return (EAssign info ltype l' r')
            ERecordSelect vinfo _ record label -> do -- Desugar a.b = c into a = {b = c | {a - b}}
                case record of
                    EVar vinfo' _ _ name -> do
                        mut <- lookupMut vinfo' name
                        unless mut (throwError (GenericAnalyzerError (syntaxInfoSourcePos info) ("Cannot assign to immutable variable '" ++ show name ++ "'")))
                    _ -> return ()
                let desugared = EAssign info () record (ERecordExtend vinfo () r label (ERecordRestrict vinfo () record label))
                inferExpr desugared
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
        let pos = syntaxInfoSourcePos info
        mexpr' <- inferExpr mexpr
        let mtype = exprType mexpr'
        (consts, pats, bexprs) <- unzip3 <$> traverse inferBranch branches
        mapM_ (constrain . Constraint pos mtype) (concat consts)
        let btypes = map exprType bexprs
        bconst <- fresh
        mapM_ (constrain . Constraint pos bconst) btypes
        return (EMatch info bconst mexpr' (zip pats bexprs))

    EBinOp info _ oper a b -> do
        a' <- inferExpr a
        b' <- inferExpr b
        let at = exprType a'
        let bt = exprType b'
        case oper of
            _ | extractName oper `elem` ["+", "-", "*", "/", "%"] -> do -- TODO
                return (EBinOp info at oper a' b')
            _ | extractName oper `elem` ["==", "!=", ">", "<", ">=", "<="] -> do
                return (EBinOp info TBool oper a' b')
            _ | extractName oper `elem` ["||", "&&"] -> do
                constrain (Constraint (syntaxInfoSourcePos info) at TBool)
                constrain (Constraint (syntaxInfoSourcePos info) bt TBool)
                return (EBinOp info TBool oper a' b')
            _ -> do
                optype <- lookupType info oper
                rt <- fresh
                let ft = TArrow [at, bt] rt
                constrain (Constraint (syntaxInfoSourcePos info) optype ft)
                return (EBinOp info rt oper a' b')

    EUnaOp info _ oper expr -> do
        a' <- inferExpr expr
        let at = exprType a'
        case oper of
            _ | extractName oper == "-" -> do
                return (EUnaOp info at oper a')
            _ -> do
                optype <- lookupType info oper
                rt <- fresh
                constrain (Constraint (syntaxInfoSourcePos info) optype (TArrow [at] rt))
                return (EUnaOp info rt oper a')

    EClosure info _ closedVars params rtann body -> throwError (GenericAnalyzerError (syntaxInfoSourcePos info) "Closures not implemented yet")

    ECall info _ expr args -> do
        a' <- inferExpr expr
        let at = exprType a'
        bs' <- traverse inferExpr args
        let bts = map exprType bs'
        rt <- fresh
        constrain (Constraint (syntaxInfoSourcePos info) at (TArrow bts rt))
        return (ECall info rt a' bs')

    ECast info targ expr -> do
        expr' <- inferExpr expr
        return (ECast info targ expr') -- TODO

    ERecordEmpty info _ -> return (ERecordEmpty info (TRecord TRowEmpty))
    
    ERecordSelect info _ record label -> do
        restRowTyp <- fresh
        fieldTyp <- fresh
        let paramTyp = TRecord (TRowExtend label fieldTyp restRowTyp)
        let returnTyp = fieldTyp
        record' <- inferExpr record
        let recordTyp = exprType record'
        constrain (Constraint (syntaxInfoSourcePos info) paramTyp recordTyp)
        return (ERecordSelect info returnTyp record' label)
    
    ERecordRestrict info _ record label -> do
        restRowTyp <- fresh
        fieldTyp <- fresh
        let paramTyp = TRecord (TRowExtend label fieldTyp restRowTyp)
        let returnTyp = TRecord restRowTyp
        record' <- inferExpr record
        let recordTyp = exprType record'
        constrain (Constraint (syntaxInfoSourcePos info) paramTyp recordTyp)
        return (ERecordRestrict info returnTyp record' label)
    
    ERecordExtend info _ expr label record -> do
        expr' <- inferExpr expr
        record' <- inferExpr record
        let etype = exprType expr'
        let rtype = exprType record'
        restRowTyp <- fresh
        fieldTyp <- fresh
        let param1Typ = fieldTyp
        let param2Typ = TRecord restRowTyp
        let returnTyp = TRecord (TRowExtend label fieldTyp restRowTyp)
        let pos = syntaxInfoSourcePos info
        constrain (Constraint pos param1Typ etype)
        constrain (Constraint pos param2Typ rtype)
        return (ERecordExtend info returnTyp expr' label record')

    EVariant info _ expr label -> do
        restRowTyp <- fresh
        variantTyp <- fresh
        let paramTyp = variantTyp
        let returnTyp = TVariant (TRowExtend label variantTyp restRowTyp)
        expr' <- inferExpr expr
        constrain (Constraint (syntaxInfoSourcePos info) paramTyp (exprType expr'))
        return (EVariant info returnTyp expr' label)

inferLit :: Lit -> Type
inferLit = \case
    LInt _ -> TInt64
    LFloat _ -> TFloat64
    LString _ -> TString
    LChar _ -> TChar
    LBool _ -> TBool
    LUnit -> TUnit

inferBranch :: (Pattern, UntypedExpr) -> Infer ([Type], Pattern, TypedExpr)
inferBranch (PWild, expr) = ([], PWild, ) <$> inferExpr expr
inferBranch (PVar name, expr) = do
    typ <- fresh
    expr' <- scoped (M.insert name (Forall [] typ)) (inferExpr expr)
    return ([typ], PVar name, expr')
inferBranch (PVariant label vname, expr) = do
    restRowTyp <- fresh
    fieldTyp <- fresh
    let variantType = TVariant (TRowExtend label fieldTyp restRowTyp)
    expr' <- scoped (M.insert vname (Forall [] fieldTyp)) (inferExpr expr)
    return ([variantType], PVariant label vname, expr')
inferBranch (PLit lit, expr) = ([inferLit lit], PLit lit, ) <$> inferExpr expr

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

exists :: Name -> Infer Bool
exists name = gets ((isJust . M.lookup name) . environment) -- Doesn't check temp env for top levels

scoped :: (Env -> Env) -> Infer a -> Infer a
scoped fn m = do
    state <- get
    let env = environment state
    put (state { environment = fn env })
    res <- m
    state' <- get
    put (state' { environment = env })
    return res

searchReturns :: TypedExpr -> [Type]
searchReturns = exprs
    where
        exprs = concatMap exprsF . universe
        exprsF (EBlock _ _ ds _) = concatMap decls ds
        exprsF x = []
        decls = concatMap declsF . universe
        declsF (DStmt s) = stmts s
        declsF x = []
        stmts = concatMap stmtsF . universe
        stmtsF (SRet e) = [exprType e]
        stmtsF x = []

-- Unification
type Solve = ExceptT AnalyzerError Identity

compose :: Substitution -> Substitution -> Substitution
compose a b = M.map (apply a) b `M.union` a

unify :: SourcePos -> Type -> Type -> Solve Substitution
unify _ a b | a == b = return M.empty
unify pos (TVar v) t = bind pos v t
unify pos t (TVar v) = bind pos v t
unify pos a@(TConst c1) b@(TConst c2)
    | c1 /= c2 = throwError (GenericAnalyzerError pos ("Type mismatch " ++ show a ++ " ~ " ++ show b))
    | otherwise = return M.empty
unify pos (TApp a1 bs1) (TApp a2 bs2) = unifyMany pos (a1 : bs1) (a2 : bs2)
unify pos (TArrow ps r) (TArrow ps2 r2) = unifyMany pos (r : ps) (r2 : ps2)
unify pos (TRecord row1) (TRecord row2) = unify pos row1 row2
unify pos (TVariant row1) (TVariant row2) = unify pos row1 row2
unify pos TRowEmpty TRowEmpty = return M.empty
unify pos a@(TRowExtend label1 ty1 restRow1) b@(TRowExtend label2 ty2 restRow2) = do
    restRow2 <- rewriteRow pos b label1 ty1
    unify pos restRow1 restRow2
-- | label1 /= label2 = throwError (GenericAnalyzerError pos ("Type mismatch " ++ show a ++ " ~ " ++ show b))
-- | otherwise = unifyMany pos [ty1, restRow1] [ty2, restRow2]
unify pos (TPtr t1) (TPtr t2) = unify pos t1 t2
unify pos a b = throwError (GenericAnalyzerError pos ("Type mismatch " ++ show a ++ " ~ " ++ show b))

unifyMany :: SourcePos -> [Type] -> [Type] -> Solve Substitution
unifyMany pos (t1 : ts1) (t2 : ts2) = do
    su1 <- unify pos t1 t2
    su2 <- unifyMany pos (apply su1 ts1) (apply su1 ts2)
    return (su2 `compose` su1)
unifyMany _ _ _ = return M.empty

rewriteRow :: SourcePos -> Type -> Text -> Type -> Solve Type
rewriteRow pos row2 label1 ty1 =
    case row2 of
        TRowEmpty -> throwError (GenericAnalyzerError pos ("Row doesn't contain label '" ++ unpack label1 ++ "'"))
        TRowExtend label2 ty2 restRow2 | label2 == label1 -> do
            su1 <- unify pos ty1 ty2
            return (apply su1 restRow2)
        TRowExtend label2 ty2 restRow2 -> do
            recurse <- rewriteRow pos restRow2 label1 ty1
            return (TRowExtend label2 ty2 recurse)
        tv@TVar {} -> return tv
        _ -> throwError (GenericAnalyzerError pos "Row type expected")

bind :: SourcePos -> TVar -> Type -> Solve Substitution
bind pos v t
    | v `S.member` tvs t = throwError (GenericAnalyzerError pos ("Infinite type " ++ show v ++ " ~ " ++ show t))
    | otherwise = return $ M.singleton v t 

solve :: Substitution -> [Constraint] -> Solve Substitution
solve s c =
    case c of
        [] -> return s
        (Constraint pos t1 t2 : cs) -> do
            s1 <- unify pos t1 t2
            let nsub = s1 `compose` s
            solve (s1 `compose` s) (apply s1 cs)

runSolve :: [Constraint] -> Either AnalyzerError Substitution
runSolve cs = runIdentity (runExceptT $ solve M.empty cs)