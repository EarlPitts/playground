module Main where

import GuessTheNumber (Decision (..), process)
import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = hspec do
  describe "process" do
    it "returns Guess if numbers match" $
      property $
        \n -> process (Just $ show n) n `shouldBe` Guessed
    it "returns Lower if number is lower" $
      property $
        \n m -> process (Just $ show ((abs n) + (abs m) + 1)) (abs n) `shouldBe` Lower
    it "returns Higher if number is higher" $
      property $
        \n m -> process (Just $ show ((abs n) - (abs m) - 1)) (abs n) `shouldBe` Higher
