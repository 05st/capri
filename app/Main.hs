{-# Language OverloadedStrings #-}

module Main where

import qualified Data.Text as T
import qualified Data.Text.IO as T
import System.Environment
import System.Directory

import Options.Applicative

import Parser
import Analyzer
import Codegen

data Options = Options
    { dirInput :: Bool
    , outFile :: FilePath
    , inPath :: FilePath
    }

options :: Parser Options
options = Options
    <$> switch (long "dir" <> short 'd' <> help "Input path is to directory")
    <*> strOption (long "out" <> short 'o' <> value "a.out" <> metavar "FILE")
    <*> strArgument (help "Source path (directory or file)" <> metavar "PATH")

main :: IO ()
main = runOpts =<< execParser (options `withInfo` infoString)
    where 
        withInfo opts desc = info (helper <*> opts) $ progDesc desc
        infoString = "Juno compiler"

runOpts :: Options -> IO ()
runOpts (Options dirInput outFile inPath) = do
    if dirInput
        then do
            files <- map (inPath ++) <$> listDirectory inPath
            inputs <- traverse T.readFile files
            compile (zip files inputs) outFile
        else do
            input <- T.readFile inPath
            compile [(inPath, input)] outFile

compile :: [(String, T.Text)] -> FilePath -> IO ()
compile inputs output = do
    case parse inputs of
        Right prog ->
            case analyze prog of
                Right annotated -> do
                    generate output annotated
                Left err -> putStrLn ("ERROR (ANALYZER): " ++ err)
        Left err -> putStrLn ("ERROR (PARSER): " ++ err)
