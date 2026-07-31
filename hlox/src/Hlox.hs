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
import Text.Parsec.Text

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

data Statement
  = Assignment String Expr
  | ExprStatement Expr
  deriving (Show, Eq)

data Expr
  = BoolLit Bool
  | StringLit String
  | NumLit Double
  | Add Expr Expr
  | Sub Expr Expr
  | Mult Expr Expr
  | Div Expr Expr
  deriving (Show, Eq)

type Script = [Statement]

p :: Parser Script
p = sepEndBy1 pStatement (char ';') <* eof

pStatement :: Parser Statement
pStatement = pAssignment <|> (ExprStatement <$> pExpression)

pIdentifier :: Parser String
pIdentifier =
  liftA2
    (:)
    (letter <|> char '_')
    (many $ alphaNum <|> char '_')

pAssignment :: Parser Statement
pAssignment = do
  string "var"
  many1 space
  name <- pIdentifier
  many1 space
  char '='
  many1 space
  expr <- pExpression
  pure $ Assignment name expr

pExpression :: Parser Expr
pExpression =
  choice
    [ pBoolLit
    , pStringLit
    , try pNumLit
    , pBinaryOp
    ]

pBoolLit :: Parser Expr
pBoolLit =
  BoolLit
    <$> ( ((string "true") *> pure True)
            <|> ((string "false") *> pure False)
        )

pStringLit :: Parser Expr
pStringLit =
  StringLit
    <$> between
      (char '"')
      (char '"')
      (many (noneOf "\""))

pNumLit :: Parser Expr
pNumLit =
  (NumLit . read) <$> do
    whole <- many1 digit
    frac <- option "" $ (:) <$> char '.' *> many1 digit
    pure $ whole <> frac

pBinaryOp :: Parser Expr
pBinaryOp = do
  l <- pExpression
  many space
  op <- oneOf ['+', '-', '/', '*']
  many space
  r <- pExpression
  pure $ case op of
    '+' -> Add l r
    '-' -> Sub l r
    '/' -> Div l r
    '*' -> Mult l r
