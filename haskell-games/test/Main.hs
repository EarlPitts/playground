module Main where

import GuessTheNumberSpec
import GridSpec
import Test.Hspec

main :: IO ()
main = hspec do
  guessTheNumberSpec
  gridSpec
