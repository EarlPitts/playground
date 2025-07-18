module GameOfLife where

import Data.List
import Control.Applicative
import Control.Monad
import Control.Concurrent

data Cell = Cell Int Int deriving (Show, Eq)

type Board = [Cell]

neighbors :: Cell -> [Cell]
neighbors c@(Cell x y) = delete c $ liftA2 Cell [succ x, pred x, x] [succ y, pred y, y]

dies :: Board -> Cell -> Bool
dies b c = numOfNeighbors < 2 || numOfNeighbors > 3
  where numOfNeighbors = length $ intersect b (neighbors c)

borns :: Board -> Cell -> Bool
borns b c = numOfNeighbors == 3
  where numOfNeighbors = length $ intersect b (neighbors c)

step :: Board -> Board
step b = filter (not . dies b) b ++ filter (borns b) candidate
  where candidate = filter (`notElem` b) (nub (b >>= neighbors))

-- Glorious graphics stuff
initBoard = [Cell 3 45, Cell 4 45, Cell 5 45, Cell 5 46, Cell 4 47]

data View = View Board Int Int

instance Show View where
  show (View b width height) = intercalate "\n" $ concat <$> flipBoard (splitBy width (markAlive <$> cells))
    where cells = liftA2 Cell [0..width-1] [0..height-1]
          markAlive c = if elem c b then "#" else " "
          splitBy n [] = []
          splitBy n l  = take n l : splitBy n (drop n l)
          flipBoard = reverse . transpose

animate :: Int -> View -> IO View
animate speed v@(View b w h) = do
  _ <- threadDelay speed
  _ <- print v
  animate speed (View (step b) w h)

main :: IO ()
main = forever (animate 200000 (View initBoard 50 50))
