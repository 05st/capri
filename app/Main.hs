{-# Language OverloadedStrings #-}
{-# Language TupleSections #-}
{-# Language BangPatterns #-}

module Main where

import Text.Printf
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Lazy.IO as LazyT
import Data.Text.Lazy.Builder
import System.Environment
import System.Directory
import System.CPUTime
import System.IO.Temp
import System.Process

import Control.Monad
import Options.Applicative

import Parser
import Analyzer
import Codegen

data Options = Options
    { dirInput :: Bool
    , cOut :: Bool
    , runOnly :: Bool
    , clang :: String
    , stlDir :: FilePath
    , outFile :: FilePath
    , inPath :: FilePath
    }

options :: Parser Options
options = Options
    <$> switch (long "dir" <> short 'd' <> help "Input path is to directory")
    <*> switch (long "c" <> short 'c' <> help "Output C")
    <*> switch (long "run" <> short 'r' <> help "Runs Capri program")
    <*> strOption (long "backend" <> short 'b' <> value "gcc" <> help "Use specified backend (default gcc)")
    <*> strOption (long "stl" <> short 's' <> value "" <> metavar "DIR" <> help "Standard library path (blank for no stl)")
    <*> strOption (long "out" <> short 'o' <> value "a.out" <> metavar "FILE" <> help "Output path")
    <*> strArgument (metavar "PATH" <> help "Source path (directory or file)")

main :: IO ()
main = runOpts =<< execParser (options `withInfo` infoString)
    where 
        withInfo opts desc = info (helper <*> opts) $ progDesc desc
        infoString = "Capri compiler"

readDir :: FilePath -> IO [(String, T.Text)]
readDir path = do
    files <- map (path ++) <$> listDirectory path
    inputs <- traverse T.readFile files
    return (zip files inputs)

runOpts :: Options -> IO ()
runOpts (Options dirInput cOut runOnly backend stlDir outFile inPath) = do
    files <- if dirInput then readDir inPath else T.readFile inPath >>= \input -> return [(inPath, input)]
    (stlFiles, noStdLib) <- if null stlDir then return ([], True) else (, False) <$> readDir stlDir
    cOutFile <- if cOut && not runOnly then return outFile else emptySystemTempFile "outcapri.c"

    output <- compile (stlFiles ++ files) noStdLib
    case output of
        Left err -> putStrLn err
        Right code | runOnly -> do
            binOutFile <- emptySystemTempFile ("outcapri-bin" ++ exeExtension)
            LazyT.writeFile cOutFile (toLazyText code)
            unless cOut (callProcess backend [cOutFile, "-o", binOutFile])
            callProcess binOutFile []
        Right code -> do
            LazyT.writeFile cOutFile (toLazyText code)
            unless cOut (callProcess backend [cOutFile, "-o", outFile])
    
compile :: [(String, T.Text)] -> Bool -> IO (Either String Builder)
compile files noStdLib = do
    putStrLn "Parsing..."
    case parse files of
        Right prog -> do
            putStrLn "Analyzing..."
            case analyze prog of
                Right annotated -> do
                    putStrLn "Generating..."
                    return $ Right (generate annotated noStdLib)
                Left err -> return $ Left ("ERROR (ANALYZER): " ++ err)
        Left err -> return $ Left ("ERROR (PARSER): " ++ err)   
