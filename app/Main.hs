module Main where

import System.Environment

import qualified Data.Text as T
import qualified Data.Text.IO as T

import Parser
import Analyzer.DependencyCheck
import Analyzer.Resolver
import Analyzer.Infer

main :: IO ()
main = do
    paths <- getArgs
    inputs <- traverse T.readFile paths
    case parse (zip paths inputs) of
        Left err -> putStrLn err
        Right program -> do
            case checkDependencies program of
                Just err -> print err
                Nothing -> case resolveProgram program of
                    Left err -> print err
                    Right resolved -> case inferProgram resolved of
                        Left err -> print err
                        Right typed -> print typed
