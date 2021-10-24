{-# Language OverloadedStrings #-}
{-# Language LambdaCase #-}
{-# Language TupleSections #-}


module Codegen where

import Control.Monad.Writer
import Control.Monad.State
import Control.Monad.Except

import qualified Data.Text as T
import qualified Data.Text.Lazy.IO as TIO
import Data.Text.Lazy.Builder

import Syntax

type Gen = ExceptT String (StateT GenState (Writer Builder))
data GenState = GenState
    { tmpVarCount :: Int
    , indent :: Int
    } deriving (Show)

generate :: FilePath -> [TypedDecl] -> IO ()
generate file decls = do
    let res = runWriter (runStateT (runExceptT (initGen decls)) (GenState {
        tmpVarCount = 0,
        indent = 0
    }))
    case res of
        ((Left err, _), _) -> print err
        (_, out) -> TIO.writeFile file (toLazyText out)

upIndent :: Gen ()
upIndent = do
    state <- get
    put (state {indent = indent state + 1})

downIndent :: Gen ()
downIndent = do
    state <- get
    put (state {indent = indent state - 1})

beforeStmt :: Gen String
beforeStmt = undefined

tmpVar :: Gen String
tmpVar = do
    state <- get
    let count = tmpVarCount state
    put (state {tmpVarCount = count + 1})
    return (names !! count)
    where
        names = map ('_' :) $ [1..] >>= flip replicateM ['a'..'z']

tellnl :: Builder -> Gen ()
tellnl = tell . (<> "\n")

initGen :: [TypedDecl] -> Gen ()
initGen decls = do
    tellnl "// Juno"
    tellnl "#include <stdint.h>"
    tellnl "#include <stdbool.h>"
    tellnl "typedef char UNIT;"
    genTopLevelDecls decls

genTopLevelDecls :: [TypedDecl] -> Gen ()
genTopLevelDecls = foldr ((>>) . genDecl) (return ())

genDecl :: TypedDecl -> Gen ()
genDecl = \case
    DFunc t name ps tann expr -> do
        let (TFunc pts rt) = t
        tell $ convertType rt
        tell $ " " <> fromText name
        return ()

convertType :: Type -> Builder
convertType = \case
    TInt8 -> "int8_t"
    TInt16 -> "int16_t"
    TInt32 -> "int32_t"
    TInt64 -> "int64_t"
    TUInt8 -> "uint8_t"
    TUInt16 -> "uint16_t"
    TUInt32 -> "uint32_t"
    TUInt64 -> "uint64_t"
    TFloat32 -> "float"
    TFloat64 -> "double"
    TPtr t -> convertType t <> "*"
    TChar -> "char"
    TBool -> "bool"
    TUnit -> "UNIT"
    other -> error (show other)
