module Main where

import System.Environment

import Parser
import Analyzer
import Codegen

main :: IO ()
main = do
    args <- getArgs
    input <- readFile (head args)
    case parse input of
        Right decls ->
            case infer decls of
                Right annotated -> print annotated
                Left err -> putStrLn err
        Left err -> print err
