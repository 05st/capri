{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TemplateHaskell #-}

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

import Control.Monad
import Options.Applicative

import qualified LLVM.AST as AST
import LLVM.Pretty

import Parser
import Analyzer.DependencyCheck
import Analyzer.Resolver
import Analyzer.Typecheck
import Codegen

data Options = Options
    { srcDir :: FilePath
    , outPath :: FilePath
    , noStl :: Bool
    }

runtimeEmbedded :: B.ByteString
runtimeEmbedded = $(embedFile "src/runtime.c")

stlEmbedded :: [(FilePath, B.ByteString)]
stlEmbedded = $(embedDir "stl")

options :: Parser Options
options = Options
    <$> strOption (long "dir" <> short 'd' <> value "./" <> metavar "DIR" <> help "Source directory")
    <*> strOption (long "out" <> short 'o' <> value "a.out" <> metavar "FILE" <> help "Output path")
    <*> switch (long "no-stl" <> help "Don't search for STL directory")

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
            cprFilePaths -> traverse T.readFile cprFilePaths >>= return . zip cprFilePaths
    readInputsFromChildDirs <- concat <$> (do
        childDirs <- filterM doesDirectoryExist filePaths
        traverse (readDir . (++ ['/'])) childDirs)
    return (readInputs ++ readInputsFromChildDirs)

runOpts :: Options -> IO ()
runOpts (Options srcDir outPath noStl) = do
    readInputs <- readDir srcDir

    let stlInputs =
            if noStl
                then []
                else map (\(fp, i) -> (fp, (cs i) :: T.Text)) stlEmbedded

    case (parse srcDir readInputs stlInputs) >>= (\p -> p <$ (mapLeft show . maybeToEither . checkDependencies) p) >>= mapLeft show . resolveProgram >>= mapLeft show . typecheckProgram of
        Left err -> putStrLn err
        Right typed -> do
            let llvmMod = generate typed
            llvmFile <- emptySystemTempFile "capri.ll"
            runtimeFile <- emptySystemTempFile "capri.c"

            handle <- openFile llvmFile ReadWriteMode
            T.hPutStrLn handle (cs $ ppllvm llvmMod)
            hClose handle

            B.writeFile runtimeFile runtimeEmbedded

            print [llvmFile, runtimeFile]

            callProcess "clang" ["-Wno-override-module", llvmFile, runtimeFile, "-O2", "-o", outPath]

maybeToEither :: Maybe a -> Either a ()
maybeToEither (Just a) = Left a
maybeToEither Nothing = Right ()

mapLeft :: (a -> b) -> Either a c -> Either b c
mapLeft f (Left x) = Left (f x)
mapLeft _ (Right x) = Right x