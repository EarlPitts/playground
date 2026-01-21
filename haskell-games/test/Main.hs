module Main where

import GuessTheNumberSpec
import Test.Hspec

main :: IO ()
main = hspec do
  guessTheNumberSpec
