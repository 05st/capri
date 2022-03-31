{-# Language LambdaCase #-}
{-# Language OverloadedStrings #-}
{-# Language TemplateHaskell #-}
{-# Language FlexibleContexts #-}

module Codegen where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Lazy.Builder
import qualified Data.Text.Lazy.IO as TIO
import qualified Data.Map as M
import Data.List

import Control.Monad.Except
import Control.Monad.State

import Control.Lens

import System.IO.Temp

import Syntax
import Name
import Type

type Gen = ExceptT Text (State GenState)
data GenState = GenState
    { _output :: Builder
    , _buffer :: Builder
    , _tmpVarCount :: Int
    , _operMap :: M.Map Name Int
    , _operCount :: Int
    }

makeLenses ''GenState

capriHeader :: Text
capriHeader = ""

generate :: TypedProgram -> IO ()
generate prog =
    case evalState (runExceptT (genProgram prog)) defaultGenState of
        Left err -> print err
        Right mods -> do
            files <- traverse (const (emptySystemTempFile "capri")) mods
            sequence_ [TIO.writeFile file (toLazyText builder) | (file, builder) <- zip files mods]
            print files
    where
        defaultGenState = GenState mempty mempty 0 mempty 0

genProgram :: TypedProgram -> Gen [Builder]
genProgram = traverse genModule

genModule :: TypedModule -> Gen Builder
genModule mod = do
    output .= mempty
    buffer .= mempty
    mapM_ genTopLvl (modTopLvls mod)
    gets _output

genTopLvl :: TypedTopLvl -> Gen ()
genTopLvl = \case
    TLFunc info (TArrow ptypes rtype) _ isOper name paramsWithAnnots _ body -> do
        nameBuilder <- if isOper then addOper name else return (convertName name)
        let (params, _) = unzip paramsWithAnnots
        let ptypes' = map convertType ptypes
        let rtype' = convertType rtype

        let paramsBuilder = mconcat (intersperse ", " $ [ptype <> " " <> fromText param | (param, ptype) <- zip params ptypes'])
        let fnDeclBuilder = rtype' <> " " <> nameBuilder <> "(" <> paramsBuilder <> ")"

        write (fnDeclBuilder <> " {\n")
        write "return 0;\n}\n"
    _ -> write "//\n"
    where
        addOper name = do
            count <- gets _operCount
            map <- gets _operMap
            operMap .= M.insert name count map
            operCount += 1
            return ("_op_" <> fromString (show count))

convertName :: Name -> Builder
convertName (Unqualified n) = fromText n
convertName (Qualified ns n) = fromText (T.intercalate "__" (ns ++ [n]))

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
    TString -> "char*"
    TChar -> "char"
    TBool -> "bool"
    TUnit -> "unit"
    TConst name -> "_t_" <> convertName name
    TVar (TV x) -> fromText x
    other -> error (show other)

-- Utility
write :: Builder -> Gen ()
write txt = do
    output <>= txt