-- https://javran.github.io/posts/2014-08-22-comonad-zipper-and-conways-game-of-life.html

module DataStructures.Zipper where

import Control.Monad
import Data.Char
import Data.Foldable

step0 = "*  >  *   *  <  **<"

waveRule :: Char -> Char -> Char -> Char
waveRule l _ r
  | fromL && fromR = 'X'
  | fromL = '>'
  | fromR = '<'
  | otherwise = ' '
  where
    fromL = l `elem` "*X>"
    fromR = r `elem` "*X<"

triplets :: [a] -> [[a]]
triplets cs@(l : m : r : _) = [l, m, r] : triplets (tail cs)
triplets _ = []

step :: [Char] -> [Char]
step cs = (\[l,m,r] -> waveRule l m r) <$> triplets (" " <> cs <> " ")

run :: String -> [String]
run = takeWhile (not . all isSpace) . iterate step

main = traverse_ putStrLn (run step0)
