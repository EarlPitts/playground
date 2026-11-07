{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Hlox (main) where

import Control.Exception hiding (TypeError)
import Control.Monad
import Control.Monad.Reader
import Data.IORef
import Data.Map
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Console.Haskeline
import System.Environment
import System.Exit
import Text.Parsec hiding (State, try)
import Prelude hiding (GT, LT)

import Data.Maybe (isJust)
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
  envRef <- newIORef mempty
  runInputT defaultSettings (loop (Env envRef Nothing))
 where
  loop env = do
    minput <- getInputLine "> "
    case minput of
      Nothing -> pure ()
      Just "exit" -> pure ()
      Just line -> do
        case parse p "" (T.pack line) of
          Right ast -> do
            result <- liftIO $ try @RuntimeError $ runReaderT (exec ast) env
            case result of
              Left err -> liftIO $ print err
              Right v -> liftIO $ print v
          Left err -> liftIO $ print err
        loop env

runScript :: FilePath -> IO ()
runScript path = do
  script <- TIO.readFile path
  run script

-- TODO set exit code in case of error

run :: Text -> IO ()
run script = case parse p "" script of
  Right ast -> do
    envRef <- liftIO $ newIORef mempty
    result <- liftIO $ try @RuntimeError $ runReaderT (exec ast) (Env envRef Nothing)
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

data RuntimeError
  = TypeError Expr
  | Undefined String
  deriving (Show, Eq)
instance Exception RuntimeError

data Env = Env
  { values :: IORef (Map String Value)
  , enclosing :: Maybe Env
  }

bind :: Env -> String -> Value -> IO ()
bind env i v = modifyIORef' (values env) (insert i v)

get :: Env -> String -> IO (Maybe Value)
get env i = do
  vs <- readIORef (values env)
  case vs !? i of
    Nothing -> case enclosing env of
      Nothing -> pure Nothing
      Just env' -> get env' i
    Just v -> pure (Just v)

isDeclared :: Env -> String -> IO Bool
isDeclared env i = do
  vs <- readIORef (values env)
  if member i vs
    then pure True
    else case enclosing env of
      Nothing -> pure False
      Just env' -> isDeclared env' i

type Interpreter a = ReaderT Env IO a

exec :: [Statement] -> Interpreter ()
exec [] = pure ()
exec ((Declaration i e) : ss) = do
  v <- eval e
  env <- ask
  liftIO $ bind env i v
  exec ss
exec ((PrintStmt e) : ss) = eval e >>= liftIO . print >> exec ss
exec ((ExprStatement e) : ss) = eval e >> exec ss
exec ((Block ss) : ss') = do
  innerEnvRef <- liftIO $ newIORef mempty
  local (\e -> Env innerEnvRef (Just e)) (exec ss) >> exec ss'

eval :: Expr -> Interpreter Value
eval (BoolLit b) = pure (BoolValue b)
eval (StringLit s) = pure (StringValue s)
eval (NumLit n) = pure (NumValue n)
eval Nil = pure NilValue
eval (Grouping e) = eval e
eval (Var i) = do
  env <- ask
  v <- liftIO $ get env i
  case v of
    Just v' -> pure v'
    Nothing -> liftIO $ throwIO $ Undefined i
eval (Assignment i e) = do
  env <- ask
  d <- liftIO $ isDeclared env i
  if d
    then do
      v <- eval e
      liftIO $ bind env i v
      pure v
    else liftIO $ throwIO $ Undefined i
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
    _ -> liftIO $ throwIO $ TypeError (Unary Minus e)
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
    _ -> liftIO $ throwIO $ TypeError (Binary LT e e')
eval (Binary LTE e e') = do
  v <- eval e
  v' <- eval e'
  case (v, v') of
    (NumValue n, NumValue n') -> pure (BoolValue (n <= n'))
    _ -> liftIO $ throwIO $ TypeError (Binary LTE e e')
eval (Binary GT e e') = do
  v <- eval e
  v' <- eval e'
  case (v, v') of
    (NumValue n, NumValue n') -> pure (BoolValue (n > n'))
    _ -> liftIO $ throwIO $ TypeError (Binary GT e e')
eval (Binary GTE e e') = do
  v <- eval e
  v' <- eval e'
  case (v, v') of
    (NumValue n, NumValue n') -> pure (BoolValue (n >= n'))
    _ -> liftIO $ throwIO $ TypeError (Binary GTE e e')
eval (Binary Add e e') = do
  v <- eval e
  v' <- eval e'
  case (v, v') of
    (NumValue n, NumValue n') -> pure (NumValue (n + n'))
    (StringValue s, StringValue s') -> pure (StringValue (s <> s'))
    _ -> liftIO $ throwIO $ TypeError (Binary Add e e')
eval (Binary Sub e e') = do
  v <- eval e
  v' <- eval e'
  case (v, v') of
    (NumValue n, NumValue n') -> pure (NumValue (n - n'))
    _ -> liftIO $ throwIO $ TypeError (Binary Sub e e')
eval (Binary Mult e e') = do
  v <- eval e
  v' <- eval e'
  case (v, v') of
    (NumValue n, NumValue n') -> pure (NumValue (n * n'))
    _ -> liftIO $ throwIO $ TypeError (Binary Mult e e')
eval (Binary Div e e') = do
  v <- eval e
  v' <- eval e'
  case (v, v') of
    (NumValue n, NumValue n') -> pure (NumValue (n / n'))
    _ -> liftIO $ throwIO $ TypeError (Binary Div e e')

-- eval e = liftIO $ throwIO $ TypeError e
