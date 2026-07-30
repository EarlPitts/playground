{-# LANGUAGE OverloadedStrings #-}

module Hlox where

import Control.Monad
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Environment
import System.Exit
import System.IO (hFlush, stdout)
import Text.Parsec
import Text.Parsec.Combinator
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
  = Skip
  | Assignment String Expression
  deriving (Show, Eq)

data Expression
  = BoolLiteral Bool
  | StringLiteral String
  deriving (Show, Eq)

type Script = [Statement]

p :: Parser Script
p = sepEndBy1 pStatement (char ';')

pStatement :: Parser Statement
pStatement = pAssignment <|> pure Skip

pAssignment :: Parser Statement
pAssignment = do
  string "var"
  many1 space
  name <- many1 letter
  many1 space
  char '='
  many1 space
  expr <- pExpression
  pure $ Assignment name expr

pExpression :: Parser Expression
pExpression =
  pBoolLiteral <|> pStringLiteral

pBoolLiteral :: Parser Expression
pBoolLiteral =
  BoolLiteral
    <$> ( ((string "true") *> pure True)
            <|> ((string "false") *> pure False)
        )

pStringLiteral :: Parser Expression
pStringLiteral =
  StringLiteral
    <$> between
      (char '"')
      (char '"')
      (many (noneOf "\""))
