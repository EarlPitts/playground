{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Imageboard (
  Config (..),
  main,
) where

import qualified Data.Aeson as A
import Data.Version (showVersion)
import qualified Data.Yaml as Yaml
import qualified Database
import qualified Logger
import qualified Web

import qualified Paths_imageboard
import System.Environment (getArgs, getProgName)
import System.Exit (exitFailure)
import qualified System.IO as IO

data Config = Config
  { cLogger :: Logger.Config
  , cWeb :: Web.Config
  , cDatabase :: Database.Config
  }

instance Semigroup Config where
  (<>) l r =
    Config
      { cLogger = cLogger l <> cLogger r
      , cWeb = cWeb l <> cWeb r
      , cDatabase = cDatabase l <> cDatabase r
      }

instance Monoid Config where
  mempty =
    Config
      { cLogger = mempty
      , cWeb = mempty
      , cDatabase = mempty
      }

instance A.FromJSON Config where
  parseJSON = A.withObject "FromJSON Main.Server.Config" $ \o ->
    Config
      <$> o A..:? "logger" A..!= mempty
      <*> o A..:? "web" A..!= mempty
      <*> o A..:? "database" A..!= mempty

main :: IO ()
main = do
  args <- getArgs
  progName <- getProgName
  case args of
    [] -> run "config.yaml"
    [configPath] -> run configPath
    _ -> do
      IO.hPutStrLn IO.stderr $ "Usage: " ++ progName ++ " <conf>"
      exitFailure

run :: FilePath -> IO ()
run configPath = do
  IO.hPutStrLn IO.stderr $
    "Booting imageboard v" ++ showVersion Paths_imageboard.version

  errOrConfig <- Yaml.decodeFileEither configPath
  Config{..} <- either (fail . show) return errOrConfig

  Logger.withHandle cLogger $ \logger ->
    Database.withHandle cDatabase $ \db ->
      Web.withHandle cWeb logger db $ \web ->
        Web.run web
