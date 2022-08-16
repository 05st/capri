{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}

module Main where

import System.Environment
import System.Directory
import System.Process
import System.FilePath
import System.IO
import System.IO.Temp

import Data.String.Conversions
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Encoding as T
import Data.FileEmbed
import qualified Data.ByteString as B
import Data.Either.Combinators
import Data.Functor

import Control.Monad
import Options.Applicative

import qualified LLVM.AST as AST
import LLVM.Pretty

import Parser
import Analyzer.DependencyCheck
import Analyzer.Resolver
import Analyzer.Typecheck
import Monomorphize
import Codegen

data Options = Options
    { srcDir :: FilePath
    , outPath :: FilePath
    , noStl :: Bool
    , fast :: Bool
    }

runtimeEmbedded :: [(FilePath, B.ByteString)]
runtimeEmbedded = $(embedDir "runtime")

stlEmbedded :: [(FilePath, B.ByteString)]
stlEmbedded = $(embedDir "stl")

options :: Parser Options
options = Options
    <$> strOption (long "dir" <> short 'd' <> value "./" <> metavar "DIR" <> help "Source directory")
    <*> strOption (long "out" <> short 'o' <> value "a.out" <> metavar "FILE" <> help "Output path")
    <*> switch (long "no-stl" <> help "Don't compile with the STL")
    <*> switch (long "fast" <> help "Optimize (aggressively) for speed")

main :: IO ()
main = runOpts =<< execParser (options `withInfo` infoString)
    where
        withInfo opts desc = info (helper <*> opts) $ progDesc desc
        infoString = "Capri Compiler"

readDir :: FilePath -> IO [(FilePath, T.Text)]
readDir path = do
    filePaths <- map (path ++) <$> listDirectory path
    readInputs <-
        case filter ((== ".capri") . takeExtension) filePaths of
            [] -> [] <$ putStrLn ("No .capri files found under " ++ path)
            cprFilePaths -> do
                putStrLn (show (length cprFilePaths) ++ " .capri file(s) found under " ++ path)
                traverse T.readFile cprFilePaths <&> zip cprFilePaths
    readInputsFromChildDirs <- concat <$> (do
        childDirs <- filterM doesDirectoryExist filePaths
        traverse (readDir . (++ ['/'])) childDirs)
    return (readInputs ++ readInputsFromChildDirs)

runOpts :: Options -> IO ()
runOpts (Options srcDir outPath noStl fast) = do
    readInputs <- readDir srcDir

    let convertUtil (fp, i) = (fp, cs i :: T.Text)
    let stlInputs =
            if noStl
                then []
                else map convertUtil stlEmbedded

    case parse srcDir readInputs stlInputs -- parse
         >>= (\p -> p <$ (mapLeft show . maybeToEither . checkDependencies) p) -- check deps
         >>= mapLeft show . resolveProgram -- resolve names
         >>= mapLeft show . typecheckProgram -- infer types
         >>= monomorphize of -- monomorphize
            Left err -> putStrLn err
            Right prog -> do
                let llvmMod = generate prog
                llvmFile <- emptySystemTempFile "capri.ll"
                llvmBcFile <- emptySystemTempFile "capri.bc"

                handle <- openFile llvmFile ReadWriteMode
                T.hPutStrLn handle (cs $ ppllvm llvmMod)
                hClose handle

                let writeRuntimeFile (fp, runtimeInput) = do
                        runtimeFile <- emptySystemTempFile ("capri" ++ takeExtension fp)
                        B.writeFile runtimeFile runtimeInput
                        return runtimeFile
                runtimeFiles <- traverse writeRuntimeFile runtimeEmbedded

                print (llvmFile : runtimeFiles)

                let optimization = ["-O3" | fast]
                callProcess "opt" ([llvmFile, "-o", llvmBcFile] ++ optimization)
                callProcess "clang" (runtimeFiles ++ [llvmBcFile, "-Wno-override-module", "-o", outPath] ++ optimization)

maybeToEither :: Maybe a -> Either a ()
maybeToEither (Just a) = Left a
maybeToEither Nothing = Right ()