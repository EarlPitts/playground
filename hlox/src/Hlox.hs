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
pExpression = pUnary

pEquality :: Parser Expr
pEquality = undefined

pFactor :: Parser Expr
pFactor = undefined

pUnary :: Parser Expr
pUnary =
  (char '!' *> pure (Unary Neg) <|> char '-' *> pure (Unary Minus))
    <*> pUnary
      <|> pPrimary

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

pBinOp :: Parser (Expr -> Expr -> Expr)
pBinOp = Binary <$> (many space *> pOp <* many space)

pOp :: Parser Op
pOp =
  choice
    [ string "==" *> pure Eq
    , string "!=" *> pure Neq
    , try (string "<=") *> pure LTE
    , string "<" *> pure LT
    , try (string ">=") *> pure GTE
    , string ">" *> pure GT
    , string "+" *> pure Add
    , string "-" *> pure Sub
    , string "*" *> pure Mult
    , string "/" *> pure Div
    , string "!" *> pure Neg -- TODO
    , string "-" *> pure Minus -- TODO
    ]
