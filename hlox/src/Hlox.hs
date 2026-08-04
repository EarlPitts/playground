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
import Prelude hiding (GT, LT)

instance Show Statement where
  show (ExprStatement expr) = show expr

instance Show Expr where
  show (BoolLit b) = show b
  show (StringLit s) = "\"" <> s <> "\""
  show (NumLit n) = show n
  show Nil = "nil"
  show (Unary op expr) = show op <> "(" <> show expr <> ")"
  show (Binary op expr expr') = "(" <> show expr <> " " <> show op <> " " <> show expr' <> ")"

instance Show Op where
  show Eq = "=="
  show Neq = "!="
  show LT = "<"
  show LTE = "<="
  show GT = ">"
  show GTE = ">="
  show Add = "+"
  show Sub = "-"
  show Mult = "*"
  show Div = "/"
  show Neg = "!"
  show Minus = "-"

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
  deriving (Eq)

data Expr
  = BoolLit Bool
  | StringLit String
  | NumLit Double
  | Nil
  | Unary Op Expr
  | Binary Op Expr Expr
  deriving (Eq)

data Op
  = Eq
  | Neq
  | LT
  | LTE
  | GT
  | GTE
  | Add
  | Sub
  | Mult
  | Div
  | Neg
  | Minus
  deriving (Eq)

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
pExpression = pEquality

pEquality :: Parser Expr
pEquality = do
  left <- pComparison
  rest <- many $ try $ do
    many space
    op <- pEqualityOp
    many space
    term <- pComparison
    pure (op, term)
  pure (foldl' (\acc (op, r) -> Binary op acc r) left rest)

pEqualityOp :: Parser Op
pEqualityOp =
  choice
    [ string "==" *> pure Eq
    , string "!=" *> pure Neq
    ]

pComparison :: Parser Expr
pComparison = do
  left <- pTerm
  rest <- many $ try $ do
    many space
    op <- pComparisonOp
    many space
    term <- pTerm
    pure (op, term)
  pure (foldl' (\acc (op, r) -> Binary op acc r) left rest)

pComparisonOp :: Parser Op
pComparisonOp =
  choice
    [ try (string "<=") *> pure LTE
    , string "<" *> pure LT
    , try (string ">=") *> pure GTE
    , string ">" *> pure GT
    ]

pTerm :: Parser Expr
pTerm = do
  left <- pFactor
  rest <- many $ try $ do
    many space
    op <- pTermOp
    many space
    factor <- pFactor
    pure (op, factor)
  pure (foldl' (\acc (op, r) -> Binary op acc r) left rest)

pTermOp :: Parser Op
pTermOp = char '-' *> pure Sub <|> char '+' *> pure Add

pFactor :: Parser Expr
pFactor = do
  left <- pUnary
  rest <- many $ try $ do
    many space
    op <- pFactorOp
    many space
    unary <- pUnary
    pure (op, unary)
  pure (foldl' (\acc (op, r) -> Binary op acc r) left rest)

pFactorOp :: Parser Op
pFactorOp = char '*' *> pure Mult <|> char '/' *> pure Div

pUnary :: Parser Expr
pUnary = Unary <$> pUnaryOp <*> pUnary <|> pPrimary

pUnaryOp :: Parser Op
pUnaryOp = char '!' *> pure Neg <|> char '-' *> pure Minus

pPrimary :: Parser Expr
pPrimary =
  choice
    [ pNil
    , pBoolLit
    , pStringLit
    , pNumLit
    , between (char '(') (char ')') pExpression
    ]

pNil :: Parser Expr
pNil = string "nil" *> pure Nil

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
    frac <- option "" $ (:) <$> char '.' <*> many1 digit
    pure $ whole <> frac
