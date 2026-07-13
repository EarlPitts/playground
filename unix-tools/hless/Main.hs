module Main where

import HLess
import Metrics
import System.Environment

main :: IO ()
main = getArgs >>= directorySummaryWithMetrics . head
