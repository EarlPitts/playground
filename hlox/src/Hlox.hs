{-# LANGUAGE OverloadedStrings #-}

module Hlox (main) where

import Control.Monad
import Control.Monad.State
import Data.Map
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Environment
import System.Exit
import System.IO (hFlush, stdout)
import Text.Parsec hiding (State)

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
  Right ast -> print (evalState (exec ast) mempty)
  Left err -> print err

data Value
  = NilValue
  | NumValue Double
  | StringValue String
  | BoolValue Bool
  deriving (Eq, Show)

data TypeError = TypeError deriving (Show)

exec :: [Statement] -> State (Map String Value) (Either TypeError Value)
exec [ExprStatement e] = pure $ eval e

eval :: Expr -> Either TypeError Value
eval (BoolLit b) = Right (BoolValue b)
eval (StringLit s) = Right (StringValue s)
eval (NumLit n) = Right (NumValue n)
eval Nil = Right NilValue
eval (Unary Neg e) = case eval e of
  Right (BoolValue b) -> Right (BoolValue (not b))
  _ -> Left TypeError
eval (Unary Minus e) = case eval e of
  Right (NumValue n) -> Right (NumValue (-n))
  _ -> Left TypeError
