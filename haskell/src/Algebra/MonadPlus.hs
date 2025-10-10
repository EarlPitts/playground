module Algebra.MonadPlus where

import Algebra.KleisliEndo
import Control.Monad

type Action = Int -> Maybe Int

die :: Maybe Int
die = mzero -- mzero is the "failure" case

setHealth :: Int -> Maybe Int
setHealth = return -- return is the "success" case

powerUp :: Int -> Maybe Int
powerUp curr = return $ succ curr

hit :: Int -> Maybe Int
hit curr = guard (newHealth > 0) >> return newHealth
  where
    newHealth = pred curr

run :: Maybe Int
run = do
  h0 <- setHealth 2
  h1 <- hit h0
  h2 <- powerUp h1
  h3 <- powerUp h2
  return h3

run' :: Maybe Int
run' = appKleisliEndo (foldMap KleisliEndo actions) 2
  where
    actions = [setHealth, hit, powerUp, powerUp]
