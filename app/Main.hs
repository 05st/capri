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
    let filePath = head args
    input <- readFile filePath
    let output = last args
    case parse filePath (T.pack input) of
        Right decls -> print decls
            {-case infer decls of
                Right annotated -> do
                    generate output annotated
                Left err -> putStrLn err-}
        Left err -> putStrLn err
