-- https://javran.github.io/posts/2014-08-22-comonad-zipper-and-conways-game-of-life.html

module DataStructures.Zipper where

step0 = "*  >  *   *  <  **< "

waveRule :: Char -> Char -> Char -> Char
waveRule _ l r
  | fromL && fromR = 'X'
  | fromL = '>'
  | fromR = '<'
  | otherwise = ' '
  where
    fromL = l `elem` "*X>"
    fromR = l `elem` "*X<"
