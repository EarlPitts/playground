-- https://blog.plover.com/prog/haskell/monad-search.html
-- https://blog.jle.im/entry/unique-sample-drawing-searches-with-list-and-statet

module Algorithms.ConstraintSolving where

import Control.Monad
import Control.Monad.State
import Data.List

--     S E N D
--   + M O R E
--   ---------
--   M O N E Y

toNumber :: [Int] -> Int
toNumber = read . concatMap show

except :: [Int] -> [Int]
except = (nums \\)
  where
    nums = [0 .. 9]

-- Simple list monad solution
solve :: [(Int, Int, Int)]
solve = do
  s <- except [0]
  e <- except [s]
  n <- except [s, e]
  d <- except [s, e, n]
  let send = toNumber [s, e, n, d]
  m <- except [s, e, n, d, 0]
  o <- except [s, e, n, d, m]
  r <- except [s, e, n, d, m, o]
  let more = toNumber [m, o, r, e]
  y <- except [s, e, n, d, m, o, r]
  let money = toNumber [m, o, n, e, y]
  guard $ send + more == money
  return (send, more, money)

select :: [a] -> [(a, [a])]
select [] = []
select (x : xs) = (x, xs) : [(y, x : ys) | (y, ys) <- select xs]

solve' :: [(Int, Int, Int)]
solve' = flip evalStateT [0..9] $ do
  s <- StateT select
  e <- StateT select
  n <- StateT select
  d <- StateT select
  m <- StateT select
  o <- StateT select
  r <- StateT select
  y <- StateT select
  guard $ s /= 0 && m /= 0
  let send = toNumber [s, e, n, d]
      more = toNumber [m, o, r, e]
      money = toNumber [m, o, n, e, y]
  guard $ send + more == money
  return (send, more, money)
