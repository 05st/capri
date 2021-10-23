{-# Language LambdaCase #-}
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
    TCon "i8" -> return AST.i8
    TCon "i16" -> return AST.i16
    TCon "i32" -> return AST.i32
    TCon "i64" -> return AST.i64
    TCon "i128" -> return AST.i128
    
    TCon "u8" -> return AST.i8
    TCon "u16" -> return AST.i16
    TCon "u32" -> return AST.i32
    TCon "u64" -> return AST.i64
    TCon "u128" -> return AST.i128

    TCon "f16" -> return AST.half
    TCon "f32" -> return AST.float
    TCon "f64" -> return AST.double
    TCon "f128" -> return AST.fp128

    TCon "char" -> return AST.i8
    TCon "bool" -> return AST.i1
    TCon "unit" -> return AST.void

    TPtr t -> AST.ptr <$> convertType t

    other -> error $ "Unknown/not implemented primitive type " ++ show other

sizeof :: MonadState CEnv m => Type -> m Word32
sizeof = \case
    TCon "i8" -> return 1
    TCon "i16" -> return 2
    TCon "i32" -> return 4
    TCon "i64" -> return 8
    TCon "i128" -> return 16
    
    TCon "u8" -> return 1
    TCon "u16" -> return 2
    TCon "u32" -> return 4
    TCon "u64" -> return 8
    TCon "u128" -> return 16

    TCon "f16" -> return 2
    TCon "f32" -> return 4
    TCon "f64" -> return 8
    TCon "f128" -> return 16

    TCon "char" -> return 1
    TCon "bool" -> return 1
    TCon "unit" -> return 0

    TPtr _ -> return 8

    other -> error $ "Unknown/not implemented primitive type " ++ show other

cgenExpr :: TypedExpr -> CGen Operand
cgenExpr (EVar t name) = gets ((Map.! name) . operands)
cgenExpr _ = undefined


