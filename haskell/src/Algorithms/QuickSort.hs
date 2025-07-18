module Algorithms.QuickSort where

quickSort :: [Int] -> [Int]
quickSort [] = []
quickSort (n:ns) = lower ++ [n] ++ upper
  where
    lower = quickSort $ filter (<= n) ns
    upper = quickSort $ filter (> n) ns
