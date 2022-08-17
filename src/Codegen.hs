{-# Language TupleSections #-}
{-# Language FlexibleContexts #-}
{-# Language LambdaCase #-}
{-# Language RecursiveDo #-}
{-# Language OverloadedStrings #-}
{-# Language MultiParamTypeClasses #-}

module Codegen where

import qualified LLVM.AST.IntegerPredicate as IP
import qualified LLVM.AST.FloatingPointPredicate as FP

import LLVM.AST (Operand)
import qualified LLVM.AST as AST
import qualified LLVM.AST.Type as AST
import qualified LLVM.AST.Constant as Const
import qualified LLVM.AST.Name as AST
import LLVM.AST.Typed

import qualified LLVM.IRBuilder.Module as L
import qualified LLVM.IRBuilder.Monad as L
import qualified LLVM.IRBuilder.Instruction as L
import qualified LLVM.IRBuilder.Constant as L
import LLVM.Prelude (ShortByteString)

import Data.String.Conversions
import qualified Data.Map as M
import qualified Data.Text as T
import Data.Text (Text)
import Data.List
import Data.String
import Data.Word

import Control.Monad.State

import Syntax
import Type
import Name

import Debug.Trace

instance ConvertibleStrings Text ShortByteString where
    convertString = fromString . T.unpack

data GenEnv = GenEnv
    { operands :: M.Map Name Operand
    , strings :: M.Map Text Operand
    , enumMap :: M.Map Name (Int, AST.Type)
    , variantMap :: M.Map (Name, Text) (Int, AST.Type)
    } deriving (Eq, Show)

registerOperand :: MonadState GenEnv m => Name -> Operand -> m ()
registerOperand name op =
    modify $ \env -> env { operands = M.insert name op (operands env) }

registerString :: MonadState GenEnv m => Text -> Operand -> m ()
registerString str op =
    modify $ \env -> env { strings = M.insert str op (strings env) }

registerEnum :: MonadState GenEnv m => Name -> Int -> AST.Type -> m ()
registerEnum name size enumType = 
    modify $ \env -> env { enumMap = M.insert name (size, enumType) (enumMap env) }

registerVariant :: MonadState GenEnv m => Name -> Text -> Int -> AST.Type -> m ()
registerVariant enumName variantLabel tag variantType = 
    modify $ \env -> env { variantMap = M.insert (enumName, variantLabel) (tag, variantType) (variantMap env) }

type LLVM = L.ModuleBuilderT (State GenEnv)
type Gen = L.IRBuilderT LLVM

generate :: TypedProgram -> AST.Module
generate prog =
    flip evalState (GenEnv { operands = M.empty, strings = M.empty, enumMap = M.empty, variantMap = M.empty })
        $ L.buildModuleT "capri"
        $ mapM_ genModule prog

genModule :: TypedModule -> LLVM ()
genModule mod = mdo
    let externs = modExterns mod
    mapM_ declareExtern externs

    -- Using some recursive-do magic to register the function operands before the functions are defined
    -- so functions can be called before they are 'defined'.
    -- there probably exists a better solution
    traverse (uncurry registerOperand) funcOperMapEntries

    funcOperMapEntries <- concat <$> traverse genTopLvl (modTopLvls mod)

    return ()
    where
        declareExtern (name, paramTypes, retType) = do
            paramTypes' <- traverse convertType paramTypes
            retType' <- convertType retType
            op <- L.extern (textToLLVMName name) paramTypes' retType'
            registerOperand (Unqualified name) op

genTopLvl :: TypedTopLvl -> LLVM [(Name, Operand)]
genTopLvl (TLFunc _ t _ isOper name params _ body) = mdo
    let (TArrow paramTypes retType) = t
    -- registerOperand name function        read the paragraph above for explanation
    (function, strs) <- (do
        retType' <- convertType retType
        params <- traverse mkParam (zip paramNames paramTypes)
        func <- L.function funcName params retType' genBody
        strings' <- gets strings
        return (func, strings'))

    [(name, function)] <$ modify (\e -> e { strings = strs })
    where
        (paramNames, _) = unzip params
        funcName = AST.mkName (cs (convertName name))
        mkParam (paramName, paramType) = (,) <$> convertType paramType <*> return (L.ParameterName (cs (convertName paramName)))
        genBody ops = do
            entry <- L.block `L.named` "entry"
            forM_ (zip ops paramNames) $ \(op, name) -> do
                addr <- L.alloca (typeOf op) Nothing 0
                L.store addr 0 op
                registerOperand name addr
            genExpr body >>= L.ret
            return ()
genTopLvl TLType {} = return [] -- nothing to compile for type aliases
genTopLvl (TLEnum _ _ enumName _ variants) = do
    largestVariantSize <- maximum <$> traverse getVariantSize variants

    let enumStructType = AST.StructureType False [AST.i8, AST.ArrayType (fromIntegral largestVariantSize) AST.i8]
    enumType <- L.typedef (textToLLVMName . T.concat $ ["enum.", convertName enumName]) (Just enumStructType)

    registerEnum enumName (largestVariantSize + 1) enumType

    [] <$ mapM_ genVariantTypes (zip variants [0..]) -- Zip with the tags
    where
        getVariantSize (_, types) = sum <$> traverse sizeofType types
        genVariantTypes ((label, types), tag) = do
            types' <- traverse convertType types
            let structType = AST.StructureType False (AST.i8 : types')
            variantType <- L.typedef (textToLLVMName . T.concat $ ["enum.", convertName enumName, ".", label]) (Just structType)

            registerVariant enumName label tag variantType

genDecl :: TypedDecl -> Gen ()
genDecl (DVar _ _ name _ expr) = do
    typ <- convertType (exprType expr)
    addr <- L.alloca typ Nothing 0

    expr' <- genExpr expr
    L.store addr 0 expr'

    registerOperand name addr

genDecl (DStmt stmt) = genStmt stmt

genStmt :: TypedStmt -> Gen ()
genStmt (SExpr expr) = () <$ genExpr expr
genStmt (SRet expr) = genExpr expr >>= L.ret
genStmt (SWhile _ cond body) = mdo
    mkTerminator (L.br whileBlock)

    whileBlock <- L.block `L.named` "while"
    cont <- genExpr cond
    mkTerminator (L.condBr cont bodyBlock exitBlock)

    bodyBlock <- L.block `L.named` "body"
    genExpr body
    mkTerminator (L.br whileBlock)

    exitBlock <- L.block `L.named` "exit"
    return ()

genLVal :: TypedExpr -> Gen Operand
genLVal (EVar _ _ name) = gets ((M.! name) . operands)
genLVal (ERecordSelect _ _ recordExpr label) = genRecordSelect recordExpr label
genLVal _ = error "Called genLVal on non-lvalue"

genExpr :: TypedExpr -> Gen Operand
genExpr (ELit _ _ lit) = genLit lit
genExpr (EVar _ _ name) = do
    addr <- gets ((M.! name) . operands)
    L.load addr 0
genExpr (EAssign _ _ lhs rhs) = do
    lhs' <- genLVal lhs
    rhs' <- genExpr rhs
    L.store lhs' 0 rhs'
    return rhs'
genExpr (EBlock _ _ decls expr) = do
    mapM_ genDecl decls
    genExpr expr
genExpr (EIf _ _ cond a b) = mdo
    mkTerminator (L.br entryBlock)

    entryBlock <- L.block `L.named` "if"
    cond' <- genExpr cond
    L.condBr cond' thenBlock elseBlock

    thenBlock <- L.block `L.named` "then"
    true <- genExpr a
    mkTerminator (L.br thenExitBlock)
    thenExitBlock <- L.block `L.named` "then_exit"
    mkTerminator (L.br exitBlock)

    elseBlock <- L.block `L.named` "else"
    false <- genExpr b
    mkTerminator (L.br elseExitBlock)
    elseExitBlock <- L.block `L.named` "else_exit"
    mkTerminator (L.br exitBlock)

    exitBlock <- L.block `L.named` "exit"
    L.phi [(true, thenExitBlock), (false, elseExitBlock)]

-- todo: rewrite this disaster
genExpr (EMatch _ _ mexpr branches) = mdo
    mexpr' <- genExpr mexpr

    (firstBlock, phis) <- genBranches mexpr' mergeBlock branches

    mergeBlock <- L.block `L.named` "merge"
    L.phi phis

    where
        genBranches _ _ [] = error "Empty match expression (semant error)"
        genBranches e m [(pat, expr)] = mdo
            cond <- genPatternCond pat e
            L.condBr cond block m -- TODO: PANIC ON NONEXHAUSTIVE MATCH EXPRESSION

            block <- L.block `L.named` "branch"

            case pat of
                (PVariant enumName label vars) -> do
                    mapM_ (\(var, index) -> do
                        (_, enumType) <- gets ((M.! enumName) . enumMap)
                        (tag, variantType) <- gets ((M.! (enumName, label)) . variantMap)

                        enumAddr <- L.alloca enumType Nothing 0
                        L.store enumAddr 0 e

                        cast <- L.bitcast enumAddr (AST.ptr variantType)
                        fieldAddr <- L.gep cast [L.int32 0, L.int32 (fromIntegral index)]
                        registerOperand var fieldAddr
                        ) (zip vars [1..])
                    expr' <- genExpr expr
                    mkTerminator (L.br m)
                (PVar name) -> do
                    registerOperand name e
                _ -> return ()

            expr' <- genExpr expr
            mkTerminator (L.br m)

            return (block, [(expr', block)])

        genBranches e m ((pat, expr) : rest) = mdo
            cond <- genPatternCond pat e
            L.condBr cond block nextBlock

            block <- L.block `L.named` "branch"

            case pat of
                (PVariant enumName label vars) -> do
                    mapM_ (\(var, index) -> do
                        (_, enumType) <- gets ((M.! enumName) . enumMap)
                        (tag, variantType) <- gets ((M.! (enumName, label)) . variantMap)

                        enumAddr <- L.alloca enumType Nothing 0
                        L.store enumAddr 0 e

                        cast <- L.bitcast enumAddr (AST.ptr variantType)
                        fieldAddr <- L.gep cast [L.int32 0, L.int32 (fromIntegral index)]
                        registerOperand var fieldAddr
                        ) (zip vars [1..])
                    expr' <- genExpr expr
                    mkTerminator (L.br m)
                (PVar name) -> do
                    registerOperand name e
                _ -> return ()

            expr' <- genExpr expr
            mkTerminator (L.br m)

            (nextBlock, phis) <- genBranches e m rest

            return (block, (expr', block) : phis)
        
        genPatternCond (PLit lit) llvmExpr = do
            lit' <- genLit lit
            case lit of
                LInt _ -> L.icmp IP.EQ lit' llvmExpr
                LFloat _ -> L.fcmp FP.OEQ lit' llvmExpr
                LBool _ -> L.icmp IP.EQ lit' llvmExpr
                LChar _ -> L.icmp IP.EQ lit' llvmExpr
                LUnit -> L.icmp IP.EQ lit' llvmExpr -- Maybe just evaluate to true?
                LString _ -> L.icmp IP.EQ lit' llvmExpr -- compare char pointers
        genPatternCond (PVar var) llvmExpr = do
            return (L.bit 1)
        genPatternCond PWild _ = do
            return (L.bit 1)
        genPatternCond (PVariant enumName label vars) llvmExpr = do
            (_, enumType) <- gets ((M.! enumName) . enumMap)
            (tag, variantType) <- gets ((M.! (enumName, label)) . variantMap)

            enumAddr <- L.alloca enumType Nothing 0
            L.store enumAddr 0 llvmExpr

            tagAddr <- L.gep enumAddr [L.int32 0, L.int32 0]
            tagExpr <- L.load tagAddr 0
            L.icmp IP.EQ tagExpr (L.int32 (fromIntegral tag))

genExpr (EBinOp _ _ operName rhs lhs) = do
    oper <- gets ((M.! operName) . operands)
    rhs' <- genExpr rhs
    lhs' <- genExpr lhs
    L.call oper [(rhs', []), (lhs', [])]
genExpr (EUnaOp _ _ operName expr) = do
    oper <- gets ((M.! operName) . operands)
    expr' <- genExpr expr
    L.call oper [(expr', [])]
genExpr EClosure {} = undefined -- TODO
genExpr (ECall _ _ expr args) = do
    args' <- traverse genExpr args
    expr' <- genLVal expr
    L.call expr' (map (,[]) args')
genExpr (ECast _ target expr) = do
    let initType = exprType expr
    expr' <- genExpr expr
    llvmTargetType <- convertType target
    error "Casts not supported yet"
genExpr (ERecordEmpty _ _) = do
    emptyType <- convertType TRecordEmpty
    addr <- L.alloca emptyType Nothing 0
    L.load addr 0
genExpr (ERecordSelect _ _ recordExpr label) = do
    addr <- genRecordSelect recordExpr label
    L.load addr 0
genExpr (ERecordRestrict _ _ expr label) = undefined
genExpr e@ERecordExtend {} = do
    -- When compiling records, the code gen just allocates memory and then proceeds to
    -- set every field. Using memcpy and constants would probably be a better idea.
    let typ = exprType e
    typ' <- convertType typ
    addr <- L.alloca typ' Nothing 0

    mapM_ (setField typ addr) (collectRecordExprFields e)
    
    L.load addr 0
    where
        setField typ addr (expr, label) = do
            let index = computeRecordFieldIndex typ label
            expr' <- genExpr expr
            fieldAddr <- L.gep addr [L.int32 0, L.int32 index]
            L.store fieldAddr 0 expr'

        collectRecordExprFields (ERecordExtend _ _ expr label next) = (expr, label) : collectRecordExprFields next
        collectRecordExprFields _ = []
genExpr (EVariant _ _ enumName variantLabel exprs) = do
    (_, enumType) <- gets ((M.! enumName) . enumMap)
    (tag, variantType) <- gets ((M.! (enumName, variantLabel)) . variantMap)

    enumAddr <- L.alloca enumType Nothing 0

    tagAddr <- L.gep enumAddr [L.int32 0, L.int32 0]
    L.store tagAddr 0 (L.int8 (fromIntegral tag)) -- Store tag

    cast <- L.bitcast enumAddr (AST.ptr variantType)

    -- Store values (the order for enum variants should remain constant so we can just zip [1..] for indices)
    -- index 0 is reserved for the tag
    mapM_ (setField cast) (zip exprs [1..])

    L.load enumAddr 0
    where
        setField enumAddr (expr, idx) = do
            expr' <- genExpr expr
            fieldAddr <- L.gep enumAddr [L.int32 0, L.int32 idx]
            L.store fieldAddr 0 expr'

genRecordSelect :: TypedExpr -> Text -> Gen Operand
genRecordSelect recordExpr label = do
    let recordType = exprType recordExpr
    expr' <- genLVal recordExpr

    let index = computeRecordFieldIndex recordType label
    L.gep expr' [L.int32 0, L.int32 index]

computeRecordFieldIndex :: Type -> Text -> Integer
computeRecordFieldIndex record label = do
    let (labels, _) = unzip (collectRecordTypeFieldsSorted record)
    case elemIndex label labels of
        Nothing -> error "computeRecordFieldIndex failed"
        Just i -> fromIntegral i

genLit :: Lit -> Gen Operand
genLit (LInt n) = return (L.int64 (fromIntegral n))
genLit (LFloat n) = return (L.double n)
genLit (LString s) = do
    strs <- gets strings
    case M.lookup s strs of
        Nothing -> do
            let name = AST.mkName (show (M.size strs) <> ".str")
            op <- L.globalStringPtr (cs s) name
            registerString s (AST.ConstantOperand op)
            return (AST.ConstantOperand op)
        Just op -> return op
genLit (LChar c) = return (L.int8 (fromIntegral (fromEnum c)))
genLit (LBool b) = return (L.bit (if b then 1 else 0))
genLit LUnit = return (L.bit 0)

-- Utility
mkTerminator :: Gen () -> Gen ()
mkTerminator instr = do
    check <- L.hasTerminator
    unless check instr

-- Size of the type in bytes
sizeofType :: MonadState GenEnv m => Type -> m Int
sizeofType = \case
    TInt8 -> return 1
    TInt16 -> return 2
    TInt32 -> return 4
    TInt64 -> return 8
    TUInt8 -> return 1
    TUInt16 -> return 2
    TUInt32 -> return 4
    TUInt64 -> return 8
    TFloat32 -> return 4
    TFloat64 -> return 8
    TChar -> return 1
    TString -> return 8 -- TODO
    TBool -> return 1
    TUnit -> return 1
    TPtr _ -> return 8
    TConst n ->
        gets (fst . (M.! n) . enumMap) -- just assume its an enum type i guess
    TVar _ -> undefined
    TApp _ _ -> undefined
    TArrow _ _ -> undefined
    TRecordEmpty -> return 1
    t@TRecordExtend {} -> do
        let (_, fieldTypes) = unzip (collectRecordTypeFieldsSorted t)
        sizes <- traverse sizeofType fieldTypes
        return (sum sizes)

-- Convert Capri types to LLVM types
convertType :: MonadState GenEnv m => Type -> m AST.Type
convertType = \case
    TInt8 -> return AST.i8
    TInt16 -> return AST.i16
    TInt32 -> return AST.i32
    TInt64 -> return AST.i64
    TUInt8 -> return AST.i8
    TUInt16 -> return AST.i16
    TUInt32 -> return AST.i32
    TUInt64 -> return AST.i64
    TFloat32 -> return AST.float
    TFloat64 -> return AST.double
    TChar -> return AST.i8
    TString -> return (AST.ptr AST.i8) -- TODO
    TBool -> return AST.i1
    TUnit -> return AST.i1
    TPtr t -> AST.ptr <$> convertType t
    TConst n ->
        gets (snd . (M.! n) . enumMap) -- and assume its an enum type here as well i guess
    TVar _ -> undefined
    TApp t _ -> undefined --convertType t
    TArrow paramTypes retType -> do
        paramTypes' <- traverse convertType paramTypes
        retType' <- convertType retType
        return (AST.FunctionType retType' paramTypes' False)
    TRecordEmpty -> return (AST.StructureType False [])
    t@TRecordExtend {} -> do
        let (fieldLabels, fieldTypes) = unzip (collectRecordTypeFieldsSorted t)

        fieldTypes' <- traverse convertType fieldTypes
        return (AST.StructureType False fieldTypes')

convertName :: Name -> Text
convertName (Unqualified unqual) = unqual
convertName (Qualified quals last) = T.intercalate "__" (quals ++ [last])

textToLLVMName :: Text -> AST.Name
textToLLVMName = AST.mkName . cs

-- Sort them so they have an ordering
collectRecordTypeFieldsSorted :: Type -> [(Text, Type)]
collectRecordTypeFieldsSorted = sort . collectRecordTypeFields