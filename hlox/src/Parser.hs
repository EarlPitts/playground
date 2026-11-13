module Parser (
  p,
  Expr (..),
  Op (..),
  Statement (..),
) where

import Data.List (intercalate)
import Text.Parsec
import Text.Parsec.Text
import Prelude hiding (GT, LT)

-- instance Show Statement where
--   show (ExprStatement expr) = show expr
--   show (PrintStmt expr) = "Print " <> show expr
--   show (Declaration var expr) = var <> " = " <> show expr
--   show (Block ss) = "{ " <> concat (fmap show ss) <> " }"
--   show (If cond t f) =
--     "if (" <> show cond <> ") " <> show t <> case f of
--       Nothing -> ""
--       Just f' -> " else " <> show f'
--
-- instance Show Expr where
--   show (BoolLit b) = show b
--   show (StringLit s) = "\"" <> s <> "\""
--   show (NumLit n) = show n
--   show Nil = "nil"
--   show (Grouping e) = "(" <> show e <> ")"
--   show (Var n) = n
--   show (Unary op expr) = show op <> "(" <> show expr <> ")"
--   show (Binary op expr expr') = "(" <> show expr <> " " <> show op <> " " <> show expr' <> ")"
--   show (Logical op e e') = case op of
--     And -> show e <> " and " <> show e'
--     Or -> show e <> " or " <> show e'
--   show (Call e []) = show e
--   show (Call e es) = show e <> "(" <> intercalate "," (fmap show es) <> ")"

-- instance Show Op where
--   show Eq = "=="
--   show Neq = "!="
--   show LT = "<"
--   show LTE = "<="
--   show GT = ">"
--   show GTE = ">="
--   show Add = "+"
--   show Sub = "-"
--   show Mult = "*"
--   show Div = "/"
--   show Neg = "!"
--   show Minus = "-"
--   show And = "and"
--   show Or = "or"

data Statement
  = Declaration String Expr
  | ExprStatement Expr
  | PrintStmt Expr
  | While Expr Statement
  | Block [Statement]
  | If Expr Statement (Maybe Statement)
  | Noop
  deriving (Eq, Show)

data Expr
  = BoolLit Bool
  | StringLit String
  | NumLit Double
  | Nil
  | Var String
  | Assignment String Expr
  | Unary Op Expr
  | Binary Op Expr Expr
  | Logical Op Expr Expr
  | Grouping Expr
  | Call Expr [Expr]
  deriving (Eq, Show)

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
  | And
  | Or
  deriving (Eq, Show)

type Script = [Statement]

p :: Parser Script
p = many pDeclaration <* eof

pDeclaration :: Parser Statement
pDeclaration = do
  many space
  decl <- pVarDeclr <|> pStatement
  many space
  pure decl

pStatement :: Parser Statement
pStatement = choice $ try <$> [pPrintStmt, pIfStmt, pWhileStmt, pForStmt, pExprStmt, pBlock]

pWhileStmt :: Parser Statement
pWhileStmt = do
  string "while"
  many1 space
  e <- between (char '(') (char ')') pExpr
  many1 space
  s <- pStatement
  pure $ While e s

pForStmt :: Parser Statement
pForStmt = do
  string "for"
  many1 space
  string "("
  many space
  init <- try pVarDeclr <|> string ";" *> pure Noop
  many space
  cond <- option (BoolLit True) (try pExpr)
  many space
  string ";"
  many space
  incr <- option Noop (ExprStatement <$> try pExpr)
  many space
  string ")"
  many1 space
  stmt <- pStatement
  many space
  pure $ Block [init, While cond (Block [stmt, incr])]

pIfStmt :: Parser Statement
pIfStmt = do
  string "if"
  many1 space
  cond <- between (char '(') (char ')') pExpr
  many1 space
  t <- pStatement
  f <- optionMaybe (try (many1 space *> string "else" *> many1 space *> pStatement))
  pure $ If cond t f

pBlock :: Parser Statement
pBlock = Block <$> between (char '{') (char '}') (many pDeclaration)

pIdentifier :: Parser String
pIdentifier =
  liftA2
    (:)
    (letter <|> char '_')
    (many $ alphaNum <|> char '_')

pVarDeclr :: Parser Statement
pVarDeclr = do
  string "var"
  many1 space
  name <- pIdentifier
  expr <- option Nil $ many space *> char '=' *> many space *> pExpr
  char ';'
  pure $ Declaration name expr

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
pExpr = pAssignment

pAssignment :: Parser Expr
pAssignment = choice $ try <$> [pAssExpr, pOr]

pOr :: Parser Expr
pOr = do
  l <- pAnd
  r <- many (try (many1 space *> string "or" *> many1 space *> pAnd))
  pure $ foldl' (Logical Or) l r

pAnd :: Parser Expr
pAnd = do
  l <- pEquality
  r <- many (try (many1 space *> string "and" *> many1 space *> pEquality))
  pure $ foldl' (Logical And) l r

pAssExpr :: Parser Expr
pAssExpr = do
  i <- pOr
  case i of
    Var i' -> do
      many space
      char '='
      many space
      e <- pAssignment
      pure (Assignment i' e)
    e -> fail $ "Invalid assignment target: " <> show e

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
  choice $
    try
      <$> [ string "<=" *> pure LTE
          , string "<" *> pure LT
          , string ">=" *> pure GTE
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
pUnary = Unary <$> pUnaryOp <*> pUnary <|> pCall

pCall :: Parser Expr
pCall = do
  callee <- pPrimary
  argsLists <- many pArgs
  foldl' (\call args -> (flip Call) args <$> call) (pure callee) argsLists

pArgs :: Parser [Expr]
pArgs = between (char '(') (char ')') (pExpr `sepBy` (char ','))

pUnaryOp :: Parser Op
pUnaryOp = char '!' *> pure Neg <|> char '-' *> pure Minus

pPrimary :: Parser Expr
pPrimary =
  choice
    [ pNil
    , pBoolLit
    , pStringLit
    , pNumLit
    , Grouping
        <$> between
          (char '(' <* many space)
          (many space *> char ')')
          pExpr
    , Var <$> pIdentifier
    ]

pNil :: Parser Expr
pNil = string "nil" *> pure Nil

pBoolLit :: Parser Expr
pBoolLit =
  BoolLit
    <$> ( ((try (string "true")) *> pure True)
            <|> ((try (string "false")) *> pure False)
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
