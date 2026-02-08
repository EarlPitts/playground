module Minesweeper.Grid where

import Control.Monad
import Control.Monad.State
import Data.Array.IArray
import Data.List (delete, intersect)
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
reveal g c = accum (\t _ -> revealTile t) g [(coord, ()) | coord <- evalState (go c) []]
  where
    go c' =
      if (g ! c') == (Tile True True)
        then pure [c']
        else do
          seen <- get
          if c' `elem` seen
            then pure []
            else case mineCount g c' of
              0 -> do
                modify (c' :)
                let ns = neighborCoords g c'
                cs <- foldM (\acc n -> (acc ++) <$> go n) [] ns
                pure $ c' : cs
              _ -> pure [c']

revealMines :: Grid -> Grid
revealMines = amap (\t -> if t.mine then t {covered = False} else t)

isMine :: Tile -> Bool
isMine t = t.mine

mineUncovered :: Grid -> Bool
mineUncovered g =
  any (\t -> t == (Tile False True)) $ elems g

revealTile :: Tile -> Tile
revealTile t = t {covered = False}

mineCount :: Grid -> Coord -> Int
mineCount g = length . filter isMine . neighbors g

neighborCoords :: Grid -> Coord -> [Coord]
neighborCoords g (r, c) = intersect ixs (indices g)
  where
    ixs = [(r - 1, c), (r + 1, c), (r, c - 1), (r, c + 1)]

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
