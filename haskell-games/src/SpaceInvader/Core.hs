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

isOutside :: Coord -> Bool
isOutside (V2 _ y) = y < 0 || y >= (width - 1)

updateProjectiles :: [Coord] -> [Coord]
updateProjectiles =
  filter (\(V2 x _) -> x /= 0) . fmap moveUp

mkCover :: Coord -> [Coord]
mkCover (V2 x y) =
  [ V2 x y
  , V2 (x - 1) y
  , V2 (x - 1) (y - 1)
  , V2 x (y - 2)
  , V2 (x - 1) (y - 2)
  ]

initCover :: [Coord]
initCover =
  concatMap
    mkCover
    [ V2 26 5
    , V2 26 15
    , V2 26 25
    ]
