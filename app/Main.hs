{-# Language OverloadedStrings #-}
{-# Language TupleSections #-}

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
    , stlDir :: FilePath
    , outFile :: FilePath
    , inPath :: FilePath
    }

options :: Parser Options
options = Options
    <$> switch (long "dir" <> short 'd' <> help "Input path is to directory")
    <*> strOption (long "stl" <> value "" <> metavar "DIR" <> help "Standard library path (blank for no stl)")
    <*> strOption (long "out" <> short 'o' <> value "a.out" <> metavar "FILE" <> help "Output path")
    <*> strArgument (metavar "PATH" <> help "Source path (directory or file)")

main :: IO ()
main = runOpts =<< execParser (options `withInfo` infoString)
    where 
        withInfo opts desc = info (helper <*> opts) $ progDesc desc
        infoString = "Juno compiler"

readDir :: FilePath -> IO [(String, T.Text)]
readDir path = do
    files <- map (path ++) <$> listDirectory path
    inputs <- traverse T.readFile files
    return (zip files inputs)

runOpts :: Options -> IO ()
runOpts (Options dirInput stlDir outFile inPath) = do
    files <- if dirInput then readDir inPath else T.readFile inPath >>= \input -> return [(inPath, input)]
    (stlFiles, noStdLib) <- if null stlDir then return ([], True) else (, False) <$> readDir stlDir
    compile (stlFiles ++ files) outFile noStdLib

compile :: [(String, T.Text)] -> FilePath -> Bool -> IO ()
compile inputs output nostl = do
    case parse inputs of
        Right prog ->
            case analyze prog of
                Right annotated -> do
                    generate output annotated nostl
                Left err -> putStrLn ("ERROR (ANALYZER): " ++ err)
        Left err -> putStrLn ("ERROR (PARSER): " ++ err)
