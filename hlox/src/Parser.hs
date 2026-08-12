module Parser (
  p,
  Expr (..),
  Op (..),
  Statement (..),
) where

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

data Statement
  = Assignment String Expr
  | ExprStatement Expr
  | PrintStmt Expr
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
p = many pStatement <* eof

pStatement :: Parser Statement
pStatement = pExprStmt <|> pPrintStmt

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
  expr <- pExpr
  pure $ Assignment name expr

pPrintStmt :: Parser Statement
pPrintStmt = do
  string "print"
  many1 space
  expr <- pExpr
  many space
  char ';'
  pure (PrintStmt expr)

pExprStmt :: Parser Statement
pExprStmt = ExprStatement <$> pExpr <* char ';'

pExpr :: Parser Expr
pExpr = pEquality

pEquality :: Parser Expr
pEquality = pBinary pComparison pEqualityOp

pEqualityOp :: Parser Op
pEqualityOp =
  choice
    [ string "==" *> pure Eq
    , string "!=" *> pure Neq
    ]

pComparison :: Parser Expr
pComparison = pBinary pTerm pComparisonOp

pComparisonOp :: Parser Op
pComparisonOp =
  choice
    [ try (string "<=") *> pure LTE
    , string "<" *> pure LT
    , try (string ">=") *> pure GTE
    , string ">" *> pure GT
    ]

pTerm :: Parser Expr
pTerm = pBinary pFactor pTermOp

pTermOp :: Parser Op
pTermOp = char '-' *> pure Sub <|> char '+' *> pure Add

pFactor :: Parser Expr
pFactor = pBinary pUnary pFactorOp

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
    , between
        (char '(' <* many space)
        (many space *> char ')')
        pExpr
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

pBinary :: Parser Expr -> Parser Op -> Parser Expr
pBinary parser pOp = do
  left <- parser
  rest <- many $ try $ do
    many space
    op <- pOp
    many space
    term <- parser
    pure (op, term)
  pure (foldl' (\acc (op, r) -> Binary op acc r) left rest)
