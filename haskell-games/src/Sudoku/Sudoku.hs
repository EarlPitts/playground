module Sudoku.Sudoku where

import Data.List (sort, transpose)

checkSolution :: [[Int]] -> Bool
checkSolution grid =
  all id $
    fmap (all id) $
      fmap checkNums
        <$> [rows, cols, diags, blocks]
  where
    rows = grid
    cols = transpose grid
    diags = [diagonal rows, diagonal cols]
    blocks = undefined

checkNums :: [Int] -> Bool
checkNums nums = sort nums == [1 .. 9]

diagonal :: [[a]] -> [a]
diagonal = fmap (\(idx, l) -> l !! idx) . zip [0 ..]
