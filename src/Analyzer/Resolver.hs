{-# Language OverloadedStrings #-}
{-# Language LambdaCase #-}

module Analyzer.Resolver where

import Data.Text (Text)
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Except
import qualified Data.Map as M
import qualified Data.Set as S

import Analyzer.AnalyzerError
import Syntax
import SyntaxInfo
import Type
import Name

import Debug.Trace

type Resolve = ExceptT AnalyzerError (ReaderT [Text] (State ResolveState))
data ResolveState = ResolveState
    { nameSet :: S.Set Name
    , pubMap :: M.Map Name Bool
    , curMod :: Maybe UntypedModule 
    } deriving (Show)

resolveProgram :: UntypedProgram -> Either AnalyzerError UntypedProgram
resolveProgram prog = evalState (runReaderT (runExceptT (traverse resolveModule prog)) []) initResolveState
    where
        initResolveState = ResolveState { nameSet = initNameSet, pubMap = initPubMap, curMod = Nothing }
        initPubMap = M.fromList $ concatMap (\mod -> concatMap (topLvlEntry mod) (modTopLvls mod)) prog
        topLvlEntry _ TLExtern {} = []
        topLvlEntry mod tl = [(head $ topLvlToName mod tl, isTopLvlPub tl)]
        initNameSet = S.fromList $ concatMap (\mod -> concatMap (topLvlToName mod) (modTopLvls mod)) prog
        -- ^ contains all of the top level declarations of each module, this is so mutual recursion works

resolveModule :: UntypedModule -> Resolve UntypedModule
resolveModule mod = do
    resolvedTopLvls <- traverse resolveTopLvl (modTopLvls mod)
    return (mod { modTopLvls = resolvedTopLvls })

resolveTopLvl :: UntypedTopLvl -> Resolve UntypedTopLvl
resolveTopLvl = \case
    TLFunc info isPub isOper name params annot expr -> TLFunc info isPub isOper name params annot <$> resolveExpr expr
    TLType info isPub name tvars typ -> TLType info isPub name tvars <$> resolveType typ
    tl@TLExtern {} -> return tl

resolveDecl :: UntypedDecl -> Resolve UntypedDecl
resolveDecl = undefined

resolveStmt :: UntypedStmt -> Resolve UntypedStmt
resolveStmt = undefined

resolveExpr :: UntypedExpr -> Resolve UntypedExpr
resolveExpr = \case
    lit@ELit {} -> return lit
    EVar info _ types name -> do
        case name of
            Unqualified unqual -> EVar info () types <$> qualifyName unqual
            Qualified {} -> EVar info () types name <$ verifyNameExists info name
    _ -> undefined

resolveType :: Type -> Resolve Type
resolveType = undefined

verifyNameExists :: SyntaxInfo -> Name -> Resolve ()
verifyNameExists info name@(Qualified prefix end) = do
    set <- gets nameSet
    -- TODO: finish this
    unless (name `S.member` set) $ verifyNameExists info (Qualified (init prefix) end)
verifyNameExists info (Unqualified unqual) =
    throwError (GenericAnalyzerError (syntaxInfoSourcePos info) "Attempt to verify unqualified name")

qualifyName :: Text -> Resolve Name
qualifyName = undefined
