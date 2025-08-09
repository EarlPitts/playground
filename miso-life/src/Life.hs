module Life where

import Grid
import Control.Comonad

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


glider :: Int -> Grid State
glider size = fromLists $ [line1, line2, line3] <> (Prelude.replicate (size - 3) (Prelude.replicate size Dead))
  where
    line1 = [Dead, Alive] <> replicate 13 Dead
    line2 = [Dead, Dead, Alive] <> replicate 12 Dead
    line3 = [Alive, Alive, Alive] <> replicate 12 Dead

initGrid :: Int -> Grid State
initGrid size = fromLists (Prelude.replicate size (Prelude.replicate size Dead))
