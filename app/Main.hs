{-# Language OverloadedStrings #-}
{-# Language TupleSections #-}
{-# Language BangPatterns #-}

module Main where

import Text.Printf
import qualified Data.Text as T
import qualified Data.Text.IO as T
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
    , clang :: String
    , stlDir :: FilePath
    , outFile :: FilePath
    , inPath :: FilePath
    }

options :: Parser Options
options = Options
    <$> switch (long "dir" <> short 'd' <> help "Input path is to directory")
    <*> switch (long "c" <> short 'c' <> help "Output C")
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
runOpts (Options dirInput cOut backend stlDir outFile inPath) = do
    files <- if dirInput then readDir inPath else T.readFile inPath >>= \input -> return [(inPath, input)]
    (stlFiles, noStdLib) <- if null stlDir then return ([], True) else (, False) <$> readDir stlDir
    cOutFile <- if cOut then return outFile else emptySystemTempFile "outcapri.c"

    putStr "Parsing..."
    start <- getCPUTime
    let !parseRes = parse (stlFiles ++ files)
    end <- getCPUTime
    printf " (%0.9f sec)\n" (fromIntegral (end - start) / (10^12) :: Double)
    case parseRes of
        Right prog -> do
            putStr "Analyzing..."
            start <- getCPUTime
            let !analyzeRes = analyze prog
            end <- getCPUTime
            printf " (%0.9f sec)\n" (fromIntegral (end - start) / (10^12) :: Double)
            case analyze prog of
                Right annotated -> do
                    putStr "Generating..."
                    start <- getCPUTime
                    generate cOutFile annotated noStdLib

                    unless cOut (callProcess backend [cOutFile, "-o", outFile])

                    end <- getCPUTime
                    printf " (%0.9f sec)\n" (fromIntegral (end - start) / (10^12) :: Double)
                Left err -> putStrLn ("ERROR (ANALYZER): " ++ err)
        Left err -> putStrLn ("ERROR (PARSER): " ++ err)   
