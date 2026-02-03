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
      0 ->
        let ns = (neighborCoords g c')
         in c' : ((ns \\ seen) >>= go (c' : seen <> ns))
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
