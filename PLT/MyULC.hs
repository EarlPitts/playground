module MyULC where

import Debug.Trace
import Control.Monad.State as S
import Data.Char
import Data.List
import Data.Map qualified as M
import Text.Parsec
import Text.Parsec.String

-- Parser

-- \x\y.x \z.x

p :: Parser RawTerm
p = appl <|> var <|> abst

abst :: Parser RawTerm
abst = do
  char '\\'
  name <- lower
  char '.'
  body <- p
  return (RawAbs name body)

appl :: Parser RawTerm
appl = do
  char '('
  t <- p
  space
  t' <- p
  char ')'
  return (RawApp t t')

var :: Parser RawTerm
var = RawVar <$> lower

type Name = Char
type Ctx = [Name]

-- Syntax
data RawTerm
  = RawVar Name
  | RawAbs Name RawTerm
  | RawApp RawTerm RawTerm
  deriving (Show, Eq)

-- Nameless Terms
data Term
  = Var Int Int
  | Abs Term
  | App Term Term
  deriving (Show, Eq)

par :: String -> RawTerm
par t = case parse p "" t of
  Right t -> t
  Left _ -> error "jfdkls"

removeNames :: Ctx -> RawTerm -> Term
removeNames ctx (RawVar x) = case elemIndex x ctx of
  Just n -> Var n (length ctx)
  Nothing -> error "Unbound var"
removeNames ctx (RawAbs name b) = Abs (removeNames (name : ctx) b)
removeNames ctx (RawApp t t') = App (removeNames ctx t) (removeNames ctx t')

restoreNames :: Ctx -> Term -> RawTerm
restoreNames ctx (Var x _) = RawVar (ctx !! x)
restoreNames ctx (Abs t) = let name = newName ctx in RawAbs name (restoreNames (name : ctx) t)
restoreNames ctx (App t t') = RawApp (restoreNames ctx t) (restoreNames ctx t')

newName :: Ctx -> Name
newName [] = 'a'
newName (n : _) = chr (succ (ord n))

shift :: Int -> Term -> Term
shift d t = walk 0 t
  where
    walk c (Var x s) = if x >= c then Var (x+d) (s+d) else Var x (s+d)
    walk c (Abs t) = Abs (walk (c+1) t)
    walk c (App t t') = App (walk c t) (walk c t')

substTop s t = shift (-1) (subst 0 (shift 1 s) t)

subst :: Int -> Term -> Term -> Term
subst i v t = walk 0 t
  where
    walk c (Var x n) = if x == i + c then shift c v else Var x n
    walk c (Abs t) = Abs (walk (c+1) t)
    walk c (App t t') = App (walk c t) (walk c t')

isVal :: Term -> Bool
isVal (Abs _) = True
isVal _ = False

reduce :: Term -> Term
reduce (App t@(Abs b) t') = if isVal t' then substTop t' b else App t (reduce t')
reduce (App t t') = App (reduce t) t'

eval :: Term -> Term
eval t
  | isVal t = t
  | otherwise = eval (reduce t)

e :: RawTerm -> RawTerm
e t = restoreNames [] (eval (removeNames [] t))
