module Main where

import System.Environment
import System.Directory
import System.Process
import System.FilePath

import qualified Data.Text as T
import qualified Data.Text.IO as T

import Control.Monad
import Options.Applicative

import Parser
import Analyzer.DependencyCheck
import Analyzer.Resolver
import Analyzer.Infer
import Codegen

data Options = Options
    { srcDir :: FilePath
    , outPath :: FilePath
    , cc :: String
    }

options :: Parser Options
options = Options
    <$> strOption (long "dir" <> short 'd' <> value "./" <> metavar "DIR" <> help "Source directory")
    <*> strOption (long "out" <> short 'o' <> value "a.out" <> metavar "FILE" <> help "Output path")
    <*> strOption (long "cc" <> short 'c' <> value "gcc" <> help "C Compiler as backend")

main :: IO ()
main = runOpts =<< execParser (options `withInfo` infoString)
    where
        withInfo opts desc = info (helper <*> opts) $ progDesc desc
        infoString = "Capri Compiler"

readDir :: FilePath -> IO [(String, T.Text)]
readDir path = do
    filePaths <- map (path ++) <$> listDirectory path
    case filter ((== ".cpr") . takeExtension) filePaths of
        [] -> [] <$ putStrLn "No .cpr files found"
        cprFilePaths -> traverse T.readFile cprFilePaths >>= return . zip cprFilePaths

runOpts :: Options -> IO ()
runOpts (Options srcDir outPath cc) = do
    inputs <- readDir srcDir

    case (parse inputs) >>= (\p -> p <$ (mapLeft show . maybeToEither . checkDependencies) p) >>= mapLeft show . resolveProgram >>= mapLeft show . inferProgram of
        Left err -> putStrLn err
        Right typed -> do
            genFiles <- generate typed
            callProcess cc (genFiles ++ ["-O2", "-o", outPath])

maybeToEither :: Maybe a -> Either a ()
maybeToEither (Just a) = Left a
maybeToEither Nothing = Right ()

mapLeft :: (a -> b) -> Either a c -> Either b c
mapLeft f (Left x) = Left (f x)
mapLeft _ (Right x) = Right x