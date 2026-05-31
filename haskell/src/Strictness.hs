{-# LANGUAGE BangPatterns #-}

module Strictness where

import Data.IORef
import Data.Time

-- fib :: [Int] -- This makes a huge difference
fib = 0 : 1 : zipWith (+) fib (tail fib)

timeFib :: IO ()
timeFib = do
  duration <- newIORef 0
  measureWith duration $ fib !! 100000
  print =<< readIORef duration

measureWith :: IORef NominalDiffTime -> a -> IO a
measureWith timeRef f = do
  start <- getCurrentTime
  let !result = f -- Note the bang
  end <- getCurrentTime

  writeIORef timeRef (diffUTCTime end start)
  pure result
