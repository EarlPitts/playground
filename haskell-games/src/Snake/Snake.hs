module Snake.Snake where

import Data.Foldable
import Data.List ((\\))
import qualified Data.List.NonEmpty as NEL
import Data.Sequence (Seq ((:|>)))
import Data.Sequence.NonEmpty as NES
import Lens.Micro ((^.))
import Linear

type Snake = NESeq Coord

type Food = Coord

type Coord = V2 Int

data Dir = L | R | D | U deriving (Show)

height, width :: Int
height = 10
width = 10

emptyCells :: Snake -> [Coord]
emptyCells s = (V2 <$> [0 .. height] <*> [0 .. width]) \\ (toList s)

snakeFromList :: NEL.NonEmpty (Int, Int) -> Snake
snakeFromList = fromList . fmap (uncurry V2)

snakeLength :: Snake -> Int
snakeLength = NES.length

snakeHead :: Snake -> Coord
snakeHead = NES.head

move :: Dir -> Snake -> Snake
move dir (currHead :<|| (rest :|> _)) = (newHead currHead dir) <| (singleton currHead) |>< rest
move dir (currHead :<|| rest) = (newHead currHead dir) :<|| rest

digested :: Snake -> [Food] -> Bool
digested _ [] = False
digested (_ :||> rear) (f : _) = rear == f

ate :: Snake -> Food -> Bool
ate (h :<|| _) f = h == f

collision :: Snake -> Bool
collision (h :<|| rest) =
  h `elem` rest
    || h ^. _x < 0
    || h ^. _x > height
    || h ^. _y < 0
    || h ^. _y > width

newHead :: Coord -> Dir -> Coord
newHead (V2 x y) = \case
  D -> V2 (x + 1) y
  U -> V2 (x - 1) y
  L -> V2 x (y - 1)
  R -> V2 x (y + 1)
