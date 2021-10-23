{-# Language LambdaCase #-}
{-# Language TupleSections #-}
{-# Language FlexibleContexts #-}

module Codegen where

import qualified LLVM.AST.IntegerPredicate as IntPred
import qualified LLVM.AST.FloatingPointPredicate as FloatPred

import LLVM.AST (Operand)
import qualified LLVM.AST as AST
import qualified LLVM.AST.Type as AST
import qualified LLVM.AST.Constant as Const
import LLVM.AST.Name
import LLVM.AST.Typed (typeOf)

import qualified LLVM.IRBuilder.Module as L
import qualified LLVM.IRBuilder.Monad as L
import qualified LLVM.IRBuilder.Instruction as L
import qualified LLVM.IRBuilder.Constant as L
import LLVM.Prelude (ShortByteString)

import qualified Data.Map as Map
import Data.Word (Word32)
import Data.Char (ord)

import Control.Monad.State

import Syntax

data CEnv = CEnv
    { operands :: Map.Map String Operand
    , strings :: Map.Map String Operand
    } deriving (Show, Eq)

registerOperand :: MonadState CEnv m => String -> Operand -> m ()
registerOperand name op =
    modify $ \env -> env { operands = Map.insert name op (operands env) }

type LLVM = L.ModuleBuilderT (State CEnv)
type CGen = L.IRBuilderT LLVM

convertType :: MonadState CEnv m => Type -> m AST.Type
convertType = \case
    TInt8 -> return AST.i8
    TInt16 -> return AST.i16
    TInt32 -> return AST.i32
    TInt64 -> return AST.i64
    TInt128 -> return AST.i128
    
    TUInt8 -> return AST.i8
    TUInt16 -> return AST.i16
    TUInt32 -> return AST.i32
    TUInt64 -> return AST.i64
    TUInt128 -> return AST.i128

    TFloat16 -> return AST.half
    TFloat32 -> return AST.float
    TFloat64 -> return AST.double
    TFloat128 -> return AST.fp128

    TChar -> return AST.i8
    TBool -> return AST.i1
    TUnit -> return AST.void

    TPtr t -> AST.ptr <$> convertType t

    other -> error $ "Unknown/not implemented primitive type " ++ show other

sizeof :: MonadState CEnv m => Type -> m Word32
sizeof = \case
    TInt8 -> return 1
    TInt16 -> return 2
    TInt32 -> return 4
    TInt64 -> return 8
    TInt128 -> return 16
    
    TUInt8 -> return 1
    TUInt16 -> return 2
    TUInt32 -> return 4
    TUInt64 -> return 8
    TUInt128 -> return 16

    TFloat16 -> return 2
    TFloat32 -> return 4
    TFloat64 -> return 8
    TFloat128 -> return 16

    TChar -> return 1
    TBool -> return 1
    TUnit -> return 0

    TPtr _ -> return 8

    other -> error $ "Unknown/not implemented primitive type " ++ show other

cgenExpr :: TypedExpr -> CGen Operand
cgenExpr = \case
    -- lvalues
    EVar _ name -> gets ((Map.! name) . operands)
    EDeref _ e -> cgenExpr e
    -- literals
    ELit _ (LInt n) -> return $ L.int32 (fromIntegral n)
    ELit _ (LFloat f) -> return $ L.double f
    ELit _ (LChar c) -> return $ L.int8 (fromIntegral (ord c))
    ELit _ (LBool b) -> return $ L.bit (if b then 1 else 0)
    -- sizeof
    ESizeof _ t -> L.int32 . fromIntegral <$> sizeof t
    -- reference
    ERef _ e -> cgenExpr e -- (e is an lvalue)
    -- binary ops
    EAssign _ l r -> do
        l' <- cgenExpr l -- l is an lvalue
        r' <- cgenExpr r
        L.store l' 0 r'
        return r'
    EBinOp _ op l r -> do
        l' <- cgenExpr l
        r' <- cgenExpr r
        case op of
            "+" -> L.add l' r'
            _ -> error $ "Custom operators not implemented fully (" ++ op ++ ")"
    -- function calls
    ECall _ f args -> do
        args' <- traverse (fmap (,[]) . cgenExpr) args
        f' <- cgenExpr f
        L.call f' args'
    -- casts
    ECast _ t e -> do
        let et = typeOfExpr e
        e' <- cgenExpr e
        llvmType <- convertType t
        case (et, t) of
            (TPtr _, TPtr _) -> L.bitcast e' llvmType
            (TInt64, TPtr _) -> L.inttoptr e' llvmType
            (TInt32, TPtr _) -> L.inttoptr e' llvmType
            (TPtr _, TInt64) -> L.ptrtoint e' llvmType
            (TPtr _, TInt32) -> L.ptrtoint e' llvmType
            (TInt32, TFloat32) -> L.sitofp e' llvmType
            _ -> error "Invalid cast"
    -- other
    other -> error $ "Unknown/not implemented expression " ++ show other

cgenStmt :: TypedStmt -> CGen ()
cgenStmt = \case
    SExpr e -> void (cgenExpr e)
    other -> error $ "Unknown/not implemented statement " ++ show other
