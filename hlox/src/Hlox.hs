{-# LANGUAGE OverloadedStrings #-}

module Hlox (main) where

import Control.Monad
import Control.Monad.State
import Data.Map
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Console.Haskeline
import System.Environment
import System.Exit
import System.IO (hFlush, stdout)
import Text.Parsec hiding (State)
import Prelude hiding (GT, LT)

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
runRepl = runInputT defaultSettings loop
 where
  loop = do
    minput <- getInputLine "> "
    case minput of
      Nothing -> return ()
      Just "exit" -> return ()
      Just line -> do
        liftIO $ run (T.pack line)
        loop

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
  Right NilValue -> Right (BoolValue True)
  Right (NumValue 0) -> Right (BoolValue True)
  Right _ -> Right (BoolValue False)
  _ -> Left TypeError
eval (Unary Minus e) = case eval e of
  Right (NumValue n) -> Right (NumValue (-n))
  _ -> Left TypeError
eval (Binary Eq e e') = case (eval e, eval e') of
  (Right v, Right v') -> Right (BoolValue (v == v'))
  _ -> Left TypeError
eval (Binary Neq e e') = case (eval e, eval e') of
  (Right v, Right v') -> Right (BoolValue (v /= v'))
  _ -> Left TypeError
eval (Binary LT e e') = case (eval e, eval e') of
  (Right (NumValue v), Right (NumValue v')) -> Right (BoolValue (v < v'))
  _ -> Left TypeError
eval (Binary LTE e e') = case (eval e, eval e') of
  (Right (NumValue v), Right (NumValue v')) -> Right (BoolValue (v <= v'))
  _ -> Left TypeError
eval (Binary GT e e') = case (eval e, eval e') of
  (Right (NumValue v), Right (NumValue v')) -> Right (BoolValue (v > v'))
  _ -> Left TypeError
eval (Binary GTE e e') = case (eval e, eval e') of
  (Right (NumValue v), Right (NumValue v')) -> Right (BoolValue (v >= v'))
  _ -> Left TypeError
eval (Binary Add e e') = case (eval e, eval e') of
  (Right (NumValue v), Right (NumValue v')) -> Right (NumValue (v + v'))
  (Right (StringValue v), Right (StringValue v')) -> Right (StringValue (v <> v'))
  _ -> Left TypeError
eval (Binary Sub e e') = case (eval e, eval e') of
  (Right (NumValue v), Right (NumValue v')) -> Right (NumValue (v - v'))
  _ -> Left TypeError
eval (Binary Mult e e') = case (eval e, eval e') of
  (Right (NumValue v), Right (NumValue v')) -> Right (NumValue (v * v'))
  _ -> Left TypeError
eval (Binary Div e e') = case (eval e, eval e') of
  (Right (NumValue v), Right (NumValue v')) -> Right (NumValue (v / v'))
  _ -> Left TypeError
eval _ = Left TypeError
