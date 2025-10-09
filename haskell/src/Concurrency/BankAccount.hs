module Concurrency.BankAccount where

import Control.Concurrent.Async
import Control.Concurrent.STM
import Data.Foldable
import Control.Monad


transfer :: Int -> TVar Int -> TVar Int -> STM ()
transfer amnt from to = do
  b1 <- readTVar from
  guard $ b1 >= amnt
  b2 <- readTVar to
  writeTVar from (b1 - amnt)
  writeTVar to (b2 + amnt)

main :: IO ()
main = do
  acc1 <- newTVarIO 100
  acc2 <- newTVarIO 200
  acc3 <- newTVarIO 0
  
  let ts = [transfer 120 acc1 acc3, transfer 50 acc2 acc1] -- will retry until condition is met
  let ts' = (\t -> orElse t (pure ())) <$> ts              -- fail without retrying

  mapConcurrently_ atomically ts'

  traverse_ (readTVarIO >=> print) [acc1, acc2, acc3]


concurrentTransfers :: IO ()
concurrentTransfers = do
  as <- traverse newTVarIO (replicate 1000 100)
  emptyAcc <- newTVarIO 0
  mapConcurrently_ (\acc -> atomically $ transfer 100 acc emptyAcc) as -- concurrent
  -- traverse_ (\acc -> atomically $ transfer 100 acc emptyAcc) as -- sequential
  traverse readTVarIO as >>= print
  readTVarIO emptyAcc >>= print
