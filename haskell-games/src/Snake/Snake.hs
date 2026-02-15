module Snake.Snake where

import qualified Data.List.NonEmpty as NE
import Data.Sequence
import Linear

type Snake = Seq Coord

type Food = Coord

type Coord = V2 Int

data Dir = L | R | D | U deriving (Show)

snakeFromList :: NE.NonEmpty (Int, Int) -> Snake
snakeFromList = fromList . fmap (uncurry V2) . NE.toList

snakeLength :: Snake -> Int
snakeLength = Data.Sequence.length

move :: Dir -> Snake -> Snake
move dir ((currHead :<| rest) :|> _) = (newHead currHead dir) :<| currHead :<| rest
move dir (currHead :<| rest) = (newHead currHead dir) :<| rest

digested :: Snake -> [Food] -> Bool
digested _ [] = False
digested (_ :|> rear) (f : _) = rear == f

ate :: Snake -> Food -> Bool
ate (h :<| _) f = h == f

newHead :: Coord -> Dir -> Coord
newHead (V2 x y) = \case
  D -> V2 (x + 1) y
  U -> V2 (x - 1) y
  L -> V2 x (y - 1)
  R -> V2 x (y + 1)
