module Main where

import System.Environment

import Parser

main :: IO ()
main = do
    args <- getArgs
    input <- readFile (head args)
    print $ parse input
