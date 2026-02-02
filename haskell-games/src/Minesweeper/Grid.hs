module Minesweeper.Grid where

import Data.Array.IArray
import Data.List (delete, intersect, (\\))
import Data.Maybe

data Tile = Tile
  { covered :: Bool,
    mine :: Bool
  }
  deriving (Show, Eq)

type Coord = (Int, Int)

type Grid = Array Coord Tile

mkGrid :: Coord -> [Tile] -> Grid
mkGrid size = listArray ((1, 1), size)

reveal :: Grid -> Coord -> Grid
reveal g c = accum (\t _ -> revealTile t) g [(coord, ()) | coord <- go [] c]
  where
    go seen c' = case mineCount g c of
      0 -> c' : (((neighborCoords g c') \\ seen) >>= go [c'])
      _ -> [c']

isMine :: Tile -> Bool
isMine t = t.mine

revealTile :: Tile -> Tile
revealTile t = t {covered = False}

mineCount :: Grid -> Coord -> Int
mineCount g = length . filter isMine . neighbors g

neighborCoords :: Grid -> Coord -> [Coord]
neighborCoords g (r, c) = intersect ixs (indices g)
  where
    ixs =
      delete
        (r, c)
        [ (x, y)
        | x <- [succ, pred, id] <*> [r],
          y <- [succ, pred, id] <*> [c]
        ]

neighbors :: Grid -> Coord -> [Tile]
neighbors g (r, c) = catMaybes $ (g !?) <$> ixs
  where
    ixs =
      delete
        (r, c)
        [ (x, y)
        | x <- [succ, pred, id] <*> [r],
          y <- [succ, pred, id] <*> [c]
        ]

arr :: Array (Int, Int) Int
arr = array ((1, 1), (3, 3)) [((x, y), 5) | x <- [1 .. 3], y <- [1 .. 3]]

arr' :: Array (Int, Int) Tile
arr' = array ((1, 1), (3, 3)) [((x, y), if (x,y) == (2,2) then (Tile True False) else (Tile True True)) | x <- [1 .. 3], y <- [1 .. 3]]
