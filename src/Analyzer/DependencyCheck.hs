{-# Language OverloadedStrings #-}
{-# Language LambdaCase #-}
{-# Language TupleSections #-}
{-# Language FlexibleContexts #-}

module Analyzer.DependencyCheck where

import Data.Text (Text, intercalate, unpack)
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Either
import Data.Maybe
import Control.Monad.State
import Control.Monad.Except

import Analyzer.AnalyzerError
import Syntax
import SyntaxInfo

import Text.Megaparsec (SourcePos)
import Debug.Trace (trace)

type ModName = [Text]
data CheckState = CheckState
    { edges :: M.Map ModName [ModName]
    , visited :: S.Set ModName
    } deriving (Show)
type Check = ExceptT AnalyzerError (State CheckState)

getFullName :: UntypedModule -> ModName
getFullName mod = modPath mod ++ [modName mod]

showModFullName :: ModName -> String
showModFullName parts = unpack (intercalate "::" parts)

checkDependencies :: UntypedProgram -> Maybe AnalyzerError
checkDependencies modules = 
    case evalState (runExceptT (checkProgram fullNames)) (CheckState { edges = initEdges, visited = initVisited }) of
        Left err -> Just err
        _ -> Nothing
    where
        fullNames = map getFullName modules
        initEdges = M.fromList (map (\mod -> (getFullName mod, map snd (modImports mod))) modules)
        initVisited = S.empty 

checkProgram :: [ModName] -> Check ()
checkProgram [] = return ()
checkProgram (mod : mods) = do
    visiteds <- gets visited
    if S.notMember mod visiteds
        then dfs [] mod
        else checkProgram mods
    where
        dfs cycle mod = do
            visit mod
            edgesMap <- gets edges
            visitedSet <- gets visited

            let modEdges = M.findWithDefault [] mod edgesMap
            let nonexistentImports = filter (`S.notMember` M.keysSet edgesMap) modEdges

            unless (null nonexistentImports) -- Verify imported modules exist
                $ throwError (NonexistentModules (map showModFullName nonexistentImports))

            when (any (`S.member` visitedSet) modEdges) -- Check for any cycles
                $ throwError (CyclicDependencyError (map showModFullName (reverse (mod : cycle))))

            mapM_ (dfs (mod : cycle)) modEdges

        visit mod = do
            state <- get
            put (state { visited = S.insert mod (visited state) })
