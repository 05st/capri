{-# LANGUAGE TupleSections #-}

module Analyzer.Typecheck where

import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text as T
import Data.Generics.Uniplate.Data
import Data.Text (pack)
import Data.Maybe
import Data.Foldable

import Control.Monad.Except
import Control.Monad.State

import qualified Analyzer.Unify as U
import Analyzer.Substitution
import Analyzer.AnalyzerError
import Name
import Syntax
import Type
import SyntaxInfo

import Debug.Trace

type TypeEnv = M.Map Name PolyType
type KindEnv = M.Map Name Kind

type Infer a = ExceptT AnalyzerError (State InferState) a
data InferState = InferState
    { typeEnv     :: TypeEnv
    , tempTypeEnv :: M.Map Name TVar -- Only for top level declarations
    , mutSet      :: S.Set Name
    , variantEnv  :: M.Map (Name, T.Text) PolyType
    , freshCount  :: Int
    , constraints :: [Constraint]
    }

typecheckProgram :: UntypedProgram -> Either AnalyzerError TypedProgram
typecheckProgram prog =
    case runState (runExceptT (preInference prog *> traverse inferModule prog)) initInferState of
        (Left err, _) -> Left err
        (Right prog', state) -> do
            subst <- U.runSolve (constraints state)
            return $ fmap (fmap (apply subst)) prog'
    where
        initInferState = InferState {
            typeEnv = M.empty,
            tempTypeEnv = M.empty,
            mutSet = S.empty,
            variantEnv = M.empty,
            freshCount = 0,
            constraints = []
        }

preInference :: UntypedProgram -> Infer ()
preInference prog = mapM_ initializeTopLvl (concatMap modTopLvls prog)
    where
        initializeTopLvl (TLFunc _ _ _ _ name _ _ _) = do
            typeVar <- fresh
            state <- get
            let TVar tv = typeVar
            put (state { tempTypeEnv = M.insert name tv (tempTypeEnv state) })
        initializeTopLvl TLType {} = return ()
        initializeTopLvl (TLEnum info _ name typeParams variants) = do
            mapM_ (insertVariantCon info name typeParams) variants

        insertVariantCon info enumName typeParams (variantLabel, variantTypes) = do
            when (any (checkInfiniteTypeSize enumName) variantTypes)
                $ throwError (GenericAnalyzerError info ("Enum " ++ show enumName ++ " has infinite size with variant '" ++ T.unpack variantLabel ++ "'"))

            let varsTypeParams = S.fromList typeParams
            let varsCon = ftv variantTypes
            let typeParams' = map TVar typeParams

            env <- gets typeEnv
            if (varsTypeParams `S.intersection` varsCon) /= varsCon
                then let undefineds = S.toList (varsCon `S.difference` varsTypeParams)
                     in throwError $ GenericAnalyzerError info ("Undefined type variables " ++ show undefineds)
                else
                    let constType =
                            case typeParams' of
                                [] -> TConst enumName
                                _ -> TApp (TConst enumName) typeParams'
                        conType =
                            case variantTypes of
                                [] -> constType
                                _ -> TArrow variantTypes constType
                        polyConType = generalize env conType
                    in insertToVariantEnv enumName variantLabel polyConType
        
        checkInfiniteTypeSize :: Name -> Type -> Bool
        checkInfiniteTypeSize enumName (TConst name) = enumName == name
        checkInfiniteTypeSize enumName (TRecordExtend _ fieldType rest) = checkInfiniteTypeSize enumName fieldType || checkInfiniteTypeSize enumName rest
        checkInfiniteTypeSize _ _ = False

inferModule :: UntypedModule -> Infer TypedModule
inferModule (Module info name path imports externs topLvls) = do
    let (externNames, externParamTypes, externRetTypes) = unzip3 externs
    let externFuncTypes = [TArrow paramTypes retType | (paramTypes, retType) <- zip externParamTypes externRetTypes ]
    let externPolyTypes = map (Forall []) externFuncTypes
    let externEnvAddition = M.fromList (zip (map Unqualified externNames) externPolyTypes)

    modifyTypeEnv (`M.union` externEnvAddition)
    topLvls' <- traverse inferTopLvl topLvls
    modifyTypeEnv (`M.difference` externEnvAddition)

    return (Module info name path imports externs topLvls')

inferTopLvl :: UntypedTopLvl -> Infer TypedTopLvl
inferTopLvl (TLFunc info _ isPub isOper name params retAnnot body) = do
    alreadyDefined <- existsInTypeEnv name
    when alreadyDefined $ throwError (RedefinitionError info (show name))

    (body', funcType) <- inferFn info name params retAnnot body
    return (TLFunc info funcType isPub isOper name params retAnnot body')

    where
        inferFn info name params retAnnot body = do
            let (paramNames, paramAnnots) = unzip params
            paramTypes <- traverse (const fresh) params
            sequence_ [forM_ paramAnnot (constrain info paramType) | (paramAnnot, paramType) <- zip paramAnnots paramTypes]

            let paramPolyTypes = map (Forall []) paramTypes
            let envAddition = M.fromList (zip paramNames paramPolyTypes) -- Don't add to mutSet so default mutability is false
            modifyTypeEnv (`M.union` envAddition)

            body' <- inferExpr body
            let retType = exprType body'

            consts <- gets constraints
            subst <- liftEither (U.runSolve consts)
            env <- gets typeEnv
            let monoType = apply subst (TArrow paramTypes retType)
                polyType = generalize env monoType -- generalize for parametric polymorphism
            
            let (TArrow paramTypes' retType') = monoType
            forM_ retAnnot (constrain info retType')
            sequence_ [forM_ paramAnnot (constrain info paramType) | (paramAnnot, paramType) <- zip paramAnnots paramTypes']

            mapM_ (constrain info retType') (searchReturns body')

            state <- get
            let tempEnv = tempTypeEnv state
            constrain info monoType (TVar (fromJust (M.lookup name tempEnv)))
            put (state { tempTypeEnv = M.delete name tempEnv })

            insertToTypeEnv name polyType
            return (body', monoType)

inferTopLvl (TLType info isPub name typeParams mainType) = return (TLType info isPub name typeParams mainType)

inferTopLvl (TLEnum info isPub name typeParams variants) = return (TLEnum info isPub name typeParams variants)
    
inferDecl :: UntypedDecl -> Infer TypedDecl
inferDecl (DVar info mut name annot expr) = do
    alreadyDefined <- existsInTypeEnv name
    when alreadyDefined $ throwError (RedefinitionError info (show name))

    expr' <- inferExpr expr
    let eType = exprType expr'

    consts <- gets constraints
    subst <- liftEither (U.runSolve consts)
    env <- gets typeEnv
    let monoType = apply subst eType
        polyType = Forall [] monoType -- Not generalized

    forM_ annot (constrain info monoType)

    insertToTypeEnv name polyType
    when mut (insertToMutSet name)

    return (DVar info mut name annot expr')
inferDecl (DStmt stmt) = DStmt <$> inferStmt stmt

inferStmt :: UntypedStmt -> Infer TypedStmt
inferStmt (SRet expr) = SRet <$> inferExpr expr
inferStmt (SWhile info cond body) = do
    cond' <- inferExpr cond
    let condType = exprType cond'
    body' <- inferExpr body
    constrain info condType TBool
    return (SWhile info cond' body')
inferStmt (SExpr expr) = SExpr <$> inferExpr expr

-- Annotates expressions with fresh type variables and generates constraints
inferExpr :: UntypedExpr -> Infer TypedExpr
inferExpr (ELit info _ lit) = return (ELit info (inferLit lit) lit)

inferExpr (EVar info _ name) = do
    typ <- lookupType info name
    return (EVar info typ name)

inferExpr (EAssign info _ lhs rhs) = do
    lhs' <- inferExpr lhs
    rhs' <- inferExpr rhs

    let lhsType = exprType lhs'
    let rhsType = exprType rhs'

    constrain info lhsType rhsType

    mutCheck <- checkMut lhs'
    case mutCheck of
        Just (True, _) -> return (EAssign info lhsType lhs' rhs')
        Just (False, name) -> throwError (GenericAnalyzerError info ("Cannot assign to immutable variable '" ++ show name ++ "'"))
        Nothing -> throwError (GenericAnalyzerError info "Cannot assign to non-lvalue")
    where
        -- Allow assigning to ERecordSelect and EVar ('lvalues')
        -- If ERecordSelect, propagate up ERecordSelects until var being modified is found
        checkMut (ERecordSelect _ _ e _) = checkMut e
        checkMut (EVar varInfo _ name) = Just . (,name) <$> lookupMut varInfo name
        checkMut _ = return Nothing

inferExpr (EBlock info _ decls expr) = do
    decls' <- traverse inferDecl decls
    expr' <- inferExpr expr
    let evalType = exprType expr'
    return (EBlock info evalType decls' expr')

inferExpr (EIf info _ cond a b) = do
    cond' <- inferExpr cond
    a' <- inferExpr a
    b' <- inferExpr b

    let condType = exprType cond'
    let aType = exprType a'
    let bType = exprType b'

    constrain info condType TBool
    constrain info aType bType

    return (EIf info aType cond' a' b')

inferExpr (EMatch info _ mexpr branches) = do
    mexpr' <- inferExpr mexpr
    let mexprType = exprType mexpr'

    (mexprConsts, pats, branchExprs) <- unzip3 <$> traverse inferBranch branches

    mapM_ (constrain info mexprType) (concat mexprConsts)

    let branchExprTypes = map exprType branchExprs
    branchType <- fresh
    mapM_ (constrain info branchType) branchExprTypes

    return (EMatch info branchType mexpr' (zip pats branchExprs))

    where
        inferBranch (PWild, expr) = ([], PWild,) <$> inferExpr expr
        inferBranch (PVar name, expr) = do
            returnType <- fresh
            insertToTypeEnv name (Forall [] returnType)
            expr' <- inferExpr expr
            return ([returnType], PVar name, expr')
        inferBranch (PVariant enumName variantLabel varNames, expr) = do
            varTypes <- traverse (const fresh) varNames
            
            let envAddition = M.fromList (zip varNames (map (Forall []) varTypes))
            modifyTypeEnv (`M.union` envAddition)

            variantConType <- lookupVariant info enumName variantLabel
            retType <- fresh

            let conType =
                    case varTypes of
                        [] -> retType
                        _ -> TArrow varTypes retType

            constrain info variantConType conType

            expr' <- inferExpr expr

            return ([retType], PVariant enumName variantLabel varNames, expr')
        inferBranch (PLit lit, expr) = ([inferLit lit], PLit lit,) <$> inferExpr expr

inferExpr (EBinOp info _ name lhs rhs) = do
    lhs' <- inferExpr lhs
    rhs' <- inferExpr rhs

    let lhsType = exprType lhs'
    let rhsType = exprType rhs'

    operType <- lookupType info name
    returnType <- fresh
    let funcType = TArrow [lhsType, rhsType] returnType

    constrain info operType funcType

    return (EBinOp info returnType name lhs' rhs')

inferExpr (EUnaOp info _ name a) = do
    a' <- inferExpr a
    let aType = exprType a'

    operType <- lookupType info name
    returnType <- fresh
    let funcType = TArrow [aType] returnType

    constrain info operType funcType

    return (EUnaOp info returnType name a')

inferExpr (EClosure info _ closedVars params retAnnot body) = do
    throwError (GenericAnalyzerError info "Closures not supported yet")

inferExpr (ECall info _ expr args) = do
    expr' <- inferExpr expr
    args' <- traverse inferExpr args

    let eType = exprType expr'
    let argTypes = map exprType args'

    returnType <- fresh
    let funcType = TArrow argTypes returnType

    constrain info eType funcType

    return (ECall info returnType expr' args')

inferExpr (ECast info target expr) = do
    expr' <- inferExpr expr
    return (ECast info target expr') -- TODO

inferExpr (ERecordEmpty info _) = return (ERecordEmpty info TRecordEmpty)

inferExpr (ERecordSelect info _ expr label) = do
    expr' <- inferExpr expr

    let eType = exprType expr'
    restType <- fresh
    fieldType <- fresh

    let typ = TRecordExtend label fieldType restType
    constrain info eType typ

    return (ERecordSelect info fieldType expr' label)

inferExpr (ERecordRestrict info _ expr label) = do
    expr' <- inferExpr expr

    let eType = exprType expr'
    restType <- fresh
    fieldType <- fresh

    let typ = TRecordExtend label fieldType restType
    constrain info eType typ

    return (ERecordRestrict info restType expr' label)

inferExpr (ERecordExtend info _ expr label rest) = do
    expr' <- inferExpr expr
    rest' <- inferExpr rest

    let eType = exprType expr'
    let rType = exprType rest'
    let retType = TRecordExtend label eType rType

    return (ERecordExtend info retType expr' label rest')

inferExpr (EVariant info _ enumName variantLabel exprs) = do
    exprs' <- traverse inferExpr exprs

    variantConType <- lookupVariant info enumName variantLabel

    let exprTypes = map exprType exprs'
    enumType <- fresh
    let conType =
            case exprTypes of
                [] -> enumType
                _ -> TArrow exprTypes enumType
    constrain info conType variantConType

    return (EVariant info enumType enumName variantLabel exprs')

-- Returns type of literal
inferLit :: Lit -> Type
inferLit (LInt _)    = TInt64
inferLit (LBool _)   = TBool
inferLit (LChar _)   = TChar
inferLit (LFloat _)  = TFloat64
inferLit LUnit       = TUnit
inferLit (LString _) = TString

-- Utility
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

constrain :: SyntaxInfo -> Type -> Type -> Infer ()
constrain synInfo typeA typeB = do
    state <- get
    let constraint = Constraint synInfo typeA typeB
    put (state { constraints = constraint : constraints state })

fresh :: Infer Type
fresh = do
    state <- get
    let count = freshCount state
    put (state { freshCount = count + 1 })
    return . TVar . TV . pack . ('_':) $ supply !! count
    where
        supply = [1..] >>= flip replicateM ['a'..'z']

generalize :: TypeEnv -> Type -> PolyType
generalize env typ = Forall (S.toList vs) typ
    where
        vs = ftv typ -- `S.difference` ftv (M.elems env)
        -- ^ THIS IS PROBABLY INCORRECT

instantiate :: PolyType -> Infer Type
instantiate (Forall vars t) = do
    newVars <- traverse (const fresh) vars
    let subst = M.fromList (zip vars newVars)
    return (apply subst t)

modifyTypeEnv :: (TypeEnv -> TypeEnv) -> Infer ()
modifyTypeEnv f = do
    state <- get
    put (state { typeEnv = f (typeEnv state) })

insertToTypeEnv :: Name -> PolyType -> Infer ()
insertToTypeEnv name t = modifyTypeEnv (M.insert name t)

insertToMutSet :: Name -> Infer ()
insertToMutSet name = do
    state <- get
    put (state { mutSet = S.insert name (mutSet state) })

-- Environment lookup
lookupVar :: SyntaxInfo -> Name -> Infer (Type, Bool)
lookupVar info name = do
    typeEnv <- gets typeEnv
    tempTypeEnv <- gets tempTypeEnv
    mutSet <- gets mutSet
    let mut = S.member name mutSet
    let typ =
            case M.lookup name typeEnv of
                Just t -> t
                Nothing -> (Forall [] . TVar) (fromJust (M.lookup name tempTypeEnv)) -- is this correct?
    (,mut) <$> instantiate typ

lookupType :: SyntaxInfo -> Name -> Infer Type
lookupType info name = fst <$> lookupVar info name

lookupMut :: SyntaxInfo -> Name -> Infer Bool
lookupMut info name = snd <$> lookupVar info name

existsInTypeEnv :: Name -> Infer Bool
existsInTypeEnv name = gets ((isJust . M.lookup name) . typeEnv) -- Doesn't check temp env for top levels

-- Looks up the type for the variant constructor
lookupVariant :: SyntaxInfo -> Name -> T.Text -> Infer Type
lookupVariant info enumName variantLabel = do
    vEnv <- gets variantEnv
    let typ = fromJust (M.lookup (enumName, variantLabel) vEnv)
    instantiate typ

insertToVariantEnv :: Name -> T.Text -> PolyType -> Infer ()
insertToVariantEnv enumName variantLabel conType = do
    state <- get
    put (state { variantEnv = M.insert (enumName, variantLabel) conType (variantEnv state) })