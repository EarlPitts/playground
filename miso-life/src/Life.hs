module Life where

import Grid
import Control.Comonad

type Board = Grid State

data State = Dead | Alive deriving (Show, Eq)

step :: Grid State -> Grid State
step = extend rule

rule :: Grid State -> State
rule g =
  if isAlive g
  then (if numOfNeighbors < 2
        then Dead
        else (if numOfNeighbors > 3 then Dead else Alive))
  else (if numOfNeighbors == 3 then Alive else Dead)
    where
      numOfNeighbors = length $ filter isAlive (adjacent g)
      isAlive cell = Alive == (focus cell)

data Coord = Coord Int Int deriving Show

makeCells :: Grid State -> [(Coord, State)]
makeCells g = do
  (i, row) <- zip [0..] (toLists g)
  (j, state) <- zip [0..] row
  return (Coord i j, state)

toggle :: State -> State
toggle Alive = Dead
toggle Dead = Alive

toggleCell :: Coord -> Board -> Board
toggleCell (Coord r c) b = maybe b id (Grid.update' r c toggle b)

glider :: Int -> Board
glider size = fromLists $ [line1, line2, line3] <> (Prelude.replicate (size - 3) (Prelude.replicate size Dead))
  where
    line1 = [Dead, Alive] <> replicate 13 Dead
    line2 = [Dead, Dead, Alive] <> replicate 12 Dead
    line3 = [Alive, Alive, Alive] <> replicate 12 Dead

initBoard :: Int -> Board
initBoard size = fromLists (Prelude.replicate size (Prelude.replicate size Dead))
