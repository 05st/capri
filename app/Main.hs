module Main where

import System.Environment

import qualified Data.Text as T
import qualified Data.Text.IO as T

import Parser
import Analyzer.DependencyCheck

main :: IO ()
main = do
    paths <- getArgs
    inputs <- traverse T.readFile paths
    case parse (zip paths inputs) of
        Left err -> putStrLn err
        Right program -> print (checkDependencies program)
