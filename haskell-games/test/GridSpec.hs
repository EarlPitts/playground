module GridSpec where

import Minesweeper.Grid
import Test.Hspec
import Test.QuickCheck

gridSpec :: SpecWith ()
gridSpec = describe "reveal" do
  -- it "returns Guess if numbers match" $
  --   property $
  --     \n -> process (Just $ show n) n `shouldBe` Guessed
  it "uncovers everything if there are no mines" $
    let covered = Tile True False
        uncovered = revealTile covered
        coveredGrid = mkGrid (5, 5) (replicate 25 covered)
        uncoveredGrid = mkGrid (5, 5) (replicate 25 uncovered)
     in (reveal coveredGrid (3, 3)) `shouldBe` uncoveredGrid
