{-# Language OverloadedStrings #-}

module Main where

import qualified Data.Text as T
import System.Environment

import Parser
import Analyzer
import Codegen

main :: IO ()
main = do
    args <- getArgs
    input <- readFile (head args)
    case parse (T.pack input) of
        Right decls ->
            case infer decls of
                Right annotated -> do
                    generate "/home/sami/testjuno.c" annotated
                Left err -> putStrLn err
        Left err -> print err
