{-# LANGUAGE OverloadedStrings #-}

module Hlox (main) where

import Control.Monad
import Control.Monad.Except
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
      Nothing -> pure ()
      Just "exit" -> pure ()
      Just line -> do
        case parse p "" (T.pack line) of
          Right ast -> do
            (result, _env) <- liftIO $ runStateT (runExceptT (exec ast)) mempty
            case result of
              Left err -> liftIO $ print err
              Right v -> liftIO $ print v
          Left err -> liftIO $ print err
        loop

runScript :: FilePath -> IO ()
runScript path = do
  script <- TIO.readFile path
  run script

-- TODO set exit code in case of error

run :: Text -> IO ()
run script = case parse p "" script of
  Right ast -> do
    (result, _env) <- runStateT (runExceptT (exec ast)) mempty
    case result of
      Left err -> print err
      Right _ -> pure ()
  Left err -> print err

data Value
  = NilValue
  | NumValue Double
  | StringValue String
  | BoolValue Bool
  deriving (Eq)

instance Show Value where
  show NilValue = "nil"
  show (NumValue d) = show d
  show (StringValue str) = str
  show (BoolValue b) = if b then "true" else "false"

data RuntimeError = TypeError Expr deriving (Show, Eq)

type Env = Map String Value
type Interpreter a = ExceptT RuntimeError (StateT Env IO) a

exec :: [Statement] -> Interpreter ()
exec [] = pure ()
exec ((ExprStatement e) : ss) = eval e >> exec ss
exec ((PrintStmt e) : ss) = eval e >>= liftIO . print >> exec ss

eval :: Expr -> Interpreter Value
eval (BoolLit b) = pure (BoolValue b)
eval (StringLit s) = pure (StringValue s)
eval (NumLit n) = pure (NumValue n)
eval Nil = pure NilValue
eval (Unary Neg e) = do
  v <- eval e
  pure $ case v of
    BoolValue b -> BoolValue (not b)
    NilValue -> BoolValue True
    NumValue 0 -> BoolValue True
    _ -> BoolValue False
eval (Unary Minus e) = do
  v <- eval e
  case v of
    NumValue n -> pure $ NumValue (-n)
    _ -> throwError $ TypeError (Unary Minus e)
eval (Binary Eq e e') = do
  v <- eval e
  v' <- eval e'
  pure $ BoolValue (v == v')
eval (Binary Neq e e') = do
  v <- eval e
  v' <- eval e'
  pure $ (BoolValue (v /= v'))
eval (Binary LT e e') = do
  v <- eval e
  v' <- eval e'
  case (v, v') of
    (NumValue n, NumValue n') -> pure (BoolValue (n < n'))
    _ -> throwError $ TypeError (Binary LT e e')
eval (Binary LTE e e') = do
  v <- eval e
  v' <- eval e'
  case (v, v') of
    (NumValue n, NumValue n') -> pure (BoolValue (n <= n'))
    _ -> throwError $ TypeError (Binary LTE e e')
eval (Binary GT e e') = do
  v <- eval e
  v' <- eval e'
  case (v, v') of
    (NumValue n, NumValue n') -> pure (BoolValue (n > n'))
    _ -> throwError $ TypeError (Binary GT e e')
eval (Binary GTE e e') = do
  v <- eval e
  v' <- eval e'
  case (v, v') of
    (NumValue n, NumValue n') -> pure (BoolValue (n >= n'))
    _ -> throwError $ TypeError (Binary GTE e e')
eval (Binary Add e e') = do
  v <- eval e
  v' <- eval e'
  case (v, v') of
    (NumValue n, NumValue n') -> pure (NumValue (n + n'))
    (StringValue s, StringValue s') -> pure (StringValue (s <> s'))
    _ -> throwError $ TypeError (Binary Add e e')
eval (Binary Sub e e') = do
  v <- eval e
  v' <- eval e'
  case (v, v') of
    (NumValue n, NumValue n') -> pure (NumValue (n - n'))
    _ -> throwError $ TypeError (Binary Sub e e')
eval (Binary Mult e e') = do
  v <- eval e
  v' <- eval e'
  case (v, v') of
    (NumValue n, NumValue n') -> pure (NumValue (n * n'))
    _ -> throwError $ TypeError (Binary Mult e e')
eval (Binary Div e e') = do
  v <- eval e
  v' <- eval e'
  case (v, v') of
    (NumValue n, NumValue n') -> pure (NumValue (n / n'))
    _ -> throwError $ TypeError (Binary Div e e')
-- eval e = throwError $ TypeError e
