{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}

module SnakeSpec where

import Data.List.NonEmpty
import qualified Data.List.NonEmpty as NE
import Snake.Snake
import Test.Hspec
import Test.QuickCheck

testSnake :: [(Int, Int)] -> Snake
testSnake = snakeFromList . NE.fromList

instance (Arbitrary a, Arbitrary b) => Arbitrary (NonEmpty (a, b)) where
  arbitrary = (:|) <$> arbitrary <*> arbitrary

deriving instance Arbitrary Dir

snakeSpec :: Spec
snakeSpec = describe "move" do
  it "length 1 works" do
    let snake = testSnake [(1, 1)]
    let expected = testSnake [(2, 1)]

    let newSnake = move D snake

    newSnake `shouldBe` expected

  it "longer works" do
    let snake = testSnake [(2, 1), (1, 1)]
    let expected = testSnake [(2, 2), (2, 1)]

    let newSnake = move R snake

    newSnake `shouldBe` expected

  it "doesn't modify the length" do
    property $
      \snakeCoords (dirs :: [Dir]) -> do
        let snake = snakeFromList snakeCoords
        let newSnake = foldr move snake dirs

        snakeLength snake `shouldBe` snakeLength newSnake
