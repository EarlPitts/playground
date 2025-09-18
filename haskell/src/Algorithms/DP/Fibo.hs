module Algorithms.DP.Fibo where

import Control.Monad.State
import Criterion.Main
import Data.Array as A
import Data.Map as M hiding ((!))
import Data.Vector as V hiding (modify)
import Debug.Trace

t :: (Show a) => a -> b -> b
t n = trace ("calling fib with " <> show n)

-- Naive racursive solution
-- Way too slow for big numbers
fib :: Int -> Int
fib 0 = t 0 1
fib 1 = t 1 1
fib n = t n $ fib (n - 1) + fib (n - 2)

-- "Iterative" solution
fib' :: Int -> Int
fib' 0 = 1
fib' 1 = 1
fib' n = go 1 1 2
  where
    go prev curr i
      | i > n = curr
      | otherwise = go curr (prev + curr) (i + 1)

-- Memoized with State
fib'' :: Int -> Int
fib'' n = evalState (go n) (M.fromList [(0, 1), (1, 1)])
  where
    go :: Int -> State (Map Int Int) Int
    go n = do
      cache <- get
      case M.lookup n cache of
        Just result -> return result
        Nothing -> do
          result <- case n of
            0 -> return 1
            1 -> return 1
            _ -> do
              a <- go (n - 1)
              b <- go (n - 2)
              return (a + b)
          modify (M.insert n result)
          return result

-- Lazy Array/Vector of thunks
-- https://jelv.is/blog/Lazy-Dynamic-Programming/
fibb :: Int -> Int
fibb max = go max
  where
    go 0 = 0
    go 1 = 1
    go n = fibs A.! (n - 1) + fibs A.! (n - 2)
    fibs = listArray (0, max) [go x | x <- [0 .. max]]

fibb' :: Int -> Int
fibb' max = go max
  where
    go 0 = 0
    go 1 = 1
    go n = fibs V.! (n - 1) + fibs V.! (n - 2)
    -- fibs = V.fromList [go x | x <- [0 .. max]]
    fibs = generate max go

benchmark :: IO ()
benchmark =
  defaultMain
    [ bgroup
        "fibonacci n=30"
        [ bench "iterative" $ whnf fib' 30,
          bench "map" $ whnf fib'' 30,
          bench "array" $ whnf fibb 30,
          bench "vector" $ whnf fibb' 30
        ],
      bgroup
        "fibonacci n=4000"
        [ bench "iterative" $ whnf fib' 4000,
          bench "map" $ whnf fib'' 4000,
          bench "array" $ whnf fibb 4000,
          bench "vector" $ whnf fibb' 4000,
          bench "iterative" $ whnf fib' 4000
        ]
    ]
