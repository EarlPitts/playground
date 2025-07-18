-- https://youtu.be/kHEriteFMb0?si=uzOzW5mfq_j-zorg

module Mocking.FunctionalMocking where

{-# LANGUAGE DeriveAnyClass #-}

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Writer
import Data.Foldable as F
import Data.List as L
import Data.Map as M

data Expr
  = Lit Int
  | Var String
  | Sum Expr Expr

evaluate :: Map String Int -> Expr -> Maybe Int
evaluate _ (Lit a) = Just a
evaluate m (Var v) = M.lookup v m
evaluate m (Sum x y) = liftA2 (+) (evaluate m x) (evaluate m y)

data Expr' a t
  = Lit' a
  | Var' t
  | Sum' (Expr' a t) (Expr' a t)
  deriving (Show, Functor, Foldable, Traversable, Applicative, Monad)

data EvalService t a m = EvalService
  { evalM :: Expr' a t -> m a,
    vars :: Expr' a t -> [t],
    check :: Expr' a (Maybe t) -> Maybe (Expr' a t),
    subst :: (t -> Expr' a t) -> Expr' a t -> Expr' a t
  }

mockService :: EvalService String Int (WriterT [String] Maybe)
mockService =
  EvalService
    { evalM = evaluate' (wrapLog vartab)
    }

realService :: EvalService String Int IO
realService =
  EvalService
    { evalM = eval''',
      vars = F.toList,
      check = sequenceA,
      subst = (=<<)
    }

evaluate' :: (Num a, Applicative m) => (t -> m a) -> Expr' a t -> m a
evaluate' _ (Lit' a) = pure a
evaluate' f (Var' v) = f v
evaluate' f (Sum' x y) = liftA2 (+) (evaluate' f x) (evaluate' f y)

vartab :: String -> Maybe Int
vartab = flip M.lookup $ M.fromList [("a", 2), ("b", 3)]

wrapLog f t = do
  tell $ L.singleton $ "Accessing " ++ show t
  lift (f t)

eval :: Map String Int -> Expr' Int String -> Maybe Int
eval m = evaluate' (`M.lookup` m)

eval' = evaluate' (const (Just 2))

eval'' :: Expr' Int String -> Writer [String] Int
eval'' = evaluate' (\t -> tell [t] >> return 2)

eval''' :: Expr' Int String -> IO Int
eval''' = evaluate' (\t -> print t >> readLn)

e1 :: Expr' Int String
e1 = Sum' (Var' "b") (Sum' (Var' "a") (Lit' 3))
