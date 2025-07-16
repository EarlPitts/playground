import Control.Concurrent
import Control.Concurrent.Async

import Control.Concurrent.STM

import Control.Monad

-- Low-level concurrency lib
a = do
  forkIO (print "hello ")
  forkIO (print "world")

-- High-level lib
f :: IO ()
f = do
  t1 <- async (print "hello ")
  t2 <- async (print "world")
  mapConcurrently print [1..100]
  wait t1
  wait t2
  return ()

-- Synchronising with MVar
-- The main thred will not be able to read from the empty MVar
-- until the other thread puts a value into it after 1 second
g :: IO ()
g = do
  a <- newEmptyMVar
  async $ threadDelay 1000000 >> putMVar a 3
  b <- takeMVar a
  print b

transfer :: Int -> TVar Int -> TVar Int -> STM ()
transfer amount from to = do
  currentFrom <- readTVar from
  if currentFrom < amount
    then retry -- This will block until 'from' is changed
    else do
      writeTVar from (currentFrom - amount)
      currentTo <- readTVar to
      writeTVar to (currentTo + amount)
