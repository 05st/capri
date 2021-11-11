module Main where

import System.Environment

import qualified Data.Text as T
import qualified Data.Text.IO as T

import Parser

main :: IO ()
main = do
    file <- head <$> getArgs
    input <- T.readFile file
    case parse input of
        Left err -> putStrLn err
        Right mod -> print mod
