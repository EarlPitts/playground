{-# LANGUAGE BangPatterns #-}
-- {-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TypeApplications #-}

module Metrics where

import Control.Concurrent
import Control.Monad
import Data.IORef
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Time

import Control.Exception
import Data.Char
import Data.Foldable (foldl', for_)
import System.IO (readFile')
import Text.Printf (printf)
import Utils

data AppMetrics = AppMetrics
  { successCount :: Int
  , failureCount :: Int
  , callDuration :: M.Map String NominalDiffTime
  }

newtype Metrics = Metrics
  { metricsStore :: IORef AppMetrics
  }

measureWith :: Metrics -> String -> a -> IO a
measureWith (Metrics metricsRef) name f = do
  start <- getCurrentTime

  let result = f

  end <- getCurrentTime
  let updateDurations = M.insertWith (+) name (diffUTCTime end start)
  modifyIORef metricsRef (\m -> m{callDuration = updateDurations m.callDuration})
  pure result

measureWith' :: Metrics -> String -> IO a -> IO a
measureWith' (Metrics metricsRef) name f = do
  start <- getCurrentTime

  result <- f

  end <- getCurrentTime
  let updateDurations = M.insertWith (+) name (diffUTCTime end start)
  modifyIORef metricsRef (\m -> m{callDuration = updateDurations m.callDuration})
  pure result

mkMetrics :: IO Metrics
mkMetrics =
  Metrics
    <$> newIORef
      ( AppMetrics
          { successCount = 0
          , failureCount = 0
          , callDuration = M.empty
          }
      )

failureTick :: Metrics -> IO ()
failureTick (Metrics storeRef) =
  modifyIORef storeRef (\m -> m{failureCount = succ m.failureCount})

successTick :: Metrics -> IO ()
successTick (Metrics storeRef) =
  modifyIORef storeRef (\m -> m{successCount = succ m.successCount})

readMetrics :: Metrics -> IO AppMetrics
readMetrics = readIORef . metricsStore

displayMetrics :: Metrics -> IO ()
displayMetrics (Metrics metricsStore) = do
  m <- readIORef metricsStore
  putStrLn ("Success count: " <> show m.successCount)
  putStrLn ("Failure count: " <> show m.failureCount)
  void $ traverse (\(k, v) -> putStr (k <> " ") >> print v) (M.toList m.callDuration)

directorySummaryWithMetrics :: FilePath -> IO ()
directorySummaryWithMetrics root = do
  metrics <- mkMetrics
  histogramRef <- newIORef (M.empty :: M.Map Char Int)
  let time = measureWith' metrics

  time "traverseDirectory" $
    traverseDirectory root $ \filePath ->
      handle @IOException (\_ -> failureTick metrics) $ do
        contents <-
          time "TIO.readFile" $
            TIO.readFile filePath

        time "word count" $
          let wordCount = length $ T.words contents
           in putStrLn $ "word count: " <> show wordCount

        time "histogram" $ do
          hist <- readIORef histogramRef
          let newHist = T.foldl' (\m c -> M.insertWith (+) c 1 m) hist contents
          writeIORef histogramRef newHist

        successTick metrics

  time "print histogram" $ do
    hist <- readIORef histogramRef
    for_ (M.toList hist) $ \(letter, count) ->
      putStrLn $ printf "      %c: %d" letter count
  displayMetrics metrics
