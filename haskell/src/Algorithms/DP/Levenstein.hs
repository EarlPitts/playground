module Algorithms.DP.Levenstein where

import Control.Monad.State
-- import Data.Vector as V hiding (modify, length, minimum)

import Criterion.Main
import Data.Array as A
import Data.Map as M hiding ((!))

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

-- Vector of thunks
-- https://jelv.is/blog/Lazy-Dynamic-Programming/
lev' :: String -> String -> Int
lev' a b = d m n
  where
    (m, n) = (length a, length b)
    a' = listArray (1, m) a
    b' = listArray (1, n) b

    d i 0 = i
    d 0 j = j
    d i j
      | a' ! i == b' ! j = ds ! (i - 1, j - 1)
      | otherwise =
          minimum
            [ ds ! (i - 1, j) + 1,
              ds ! (i, j - 1) + 1,
              ds ! (i - 1, j - 1) + 1
            ]

    ds = listArray bounds [d i j | (i, j) <- range bounds]
    bounds = ((0, 0), (m, n))

benchmark :: IO ()
benchmark =
  defaultMain
    [ bgroup
        "levenstein a=fjadsklfjdsalkfjsa b=m,vcxn,v,mncx,mvcxn"
        [ bench "state memoized" $ whnf (lev "fjadsklfjdsalkfjsa") "m,vcxn,v,mncx,mvcxn",
          bench "array" $ whnf (lev' "fjadsklfjdsalkfjsa") "m,vcxn,v,mncx,mvcxn"
        ]
    ]
