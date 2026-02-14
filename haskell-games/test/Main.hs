module Main where

import GuessTheNumberSpec
import SnakeSpec
import GridSpec
import Test.Hspec

main :: IO ()
main = hspec do
  guessTheNumberSpec
  gridSpec
  snakeSpec
