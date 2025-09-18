module Algorithms.DP.Levenstein where

import Control.Monad.State
import Data.Map as M

-- The brute force solution is way too slow
-- It's basically a ternary tree, so longer
-- inputs are intractable
levenstein :: String -> String -> Int
levenstein [] b = length b
levenstein a [] = length a
levenstein (a : as) (b : bs)
  | a == b = levenstein as bs
  | otherwise =
      minimum
        [ levenstein as bs + 1,
          levenstein (a : as) bs + 1,
          levenstein as (b : bs) + 1
        ]

-- Memoized version
lev :: String -> String -> Int
lev s1 s2 = evalState (go s1 s2) M.empty
  where
    go :: String -> String -> State (Map (String, String) Int) Int
    go [] b = return $ length b
    go a [] = return $ length a
    go (a : as) (b : bs)
      | a == b = do
          cache <- get
          case M.lookup (as, bs) cache of
            Just res -> return res
            Nothing -> do
              res <- go as bs
              modify $ insert (as, bs) res
              return res
      | otherwise = do
          cache <- get
          case M.lookup (as, bs) cache of
            Just res -> return res
            Nothing -> do
              subs <- go as bs
              del <- go (a : as) bs
              ins <- go as (b : bs)
              let res = minimum $ succ <$> [subs, del, ins]
              modify $ insert (as, bs) res
              return res
