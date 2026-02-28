module SpaceInvader.Core where

import Linear (V2 (V2))

type Coord = V2 Int

height, width :: Int
height = 30
width = 30

moveRight :: Coord -> Coord
moveRight (V2 x y) = V2 x (y + 1)

moveLeft :: Coord -> Coord
moveLeft (V2 x y) = V2 x (y - 1)

moveUp :: Coord -> Coord
moveUp (V2 x y) = V2 (x - 1) y

shoot :: Coord -> [Coord] -> [Coord]
shoot ship ps = moveUp ship : ps

updateProjectiles :: [Coord] -> [Coord]
updateProjectiles = fmap moveUp
