module Main (main) where

import MonadThrowSuite
import ValidateSuite

main :: IO ()
main = do
  MonadThrowSuite.tests
  ValidateSuite.tests
