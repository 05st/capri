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
    let filePaths = init args
    inputs <- traverse readFile filePaths
    let output = last args
    case parse (zip filePaths (map T.pack inputs)) of
        Right prog ->
            case analyze prog of
                Right annotated -> do
                    generate output annotated
                Left err -> putStrLn ("ERROR (ANALYZER): " ++ err)
        Left err -> putStrLn ("ERROR (PARSER): " ++ err)
