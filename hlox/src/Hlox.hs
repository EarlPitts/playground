{-# LANGUAGE OverloadedStrings #-}

module Hlox (main) where

import Control.Monad
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Environment
import System.Exit
import System.IO (hFlush, stdout)
import Text.Parsec

import Parser

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> runRepl
    [fileName] -> runScript fileName
    _otherwise -> do
      putStrLn "Usage: hlox [script]"
      exitWith (ExitFailure 64)

runRepl :: IO ()
runRepl = do
  TIO.putStr "> "
  hFlush stdout
  line <- TIO.getLine
  unless (T.null line) $ do
    run line
    runRepl

runScript :: FilePath -> IO ()
runScript path = do
  script <- TIO.readFile path
  run script

run :: Text -> IO ()
run script = case parse p "" script of
  Right ast -> print ast
  Left err -> print err
