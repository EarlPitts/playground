module DataStructures.Grid where

import Control.Comonad
import Control.Monad
import Data.Char
import Data.Foldable
import Data.List
import DataStructures.Zipper as Z

newtype Grid a = Grid {runGrid :: Zipper (Zipper a)}
  deriving (Functor)

moveUp :: Grid a -> Grid a
moveUp (Grid z) = Grid $ Z.moveLeft z

moveDown :: Grid a -> Grid a
moveDown (Grid z) = Grid $ Z.moveRight z

moveLeft :: Grid a -> Grid a
moveLeft (Grid z) = Grid $ Z.update Z.moveLeft z

moveRight :: Grid a -> Grid a
moveRight (Grid z) = Grid $ Z.update Z.moveRight z

safeUp :: Grid a -> Maybe (Grid a)
safeUp (Grid z) = Grid <$> Z.safeLeft z

safeDown :: Grid a -> Maybe (Grid a)
safeDown (Grid z) = Grid <$> Z.safeRight z

safeLeft :: Grid a -> Maybe (Grid a)
safeLeft (Grid z) = case Z.safeLeft $ Z.focus z of
  Nothing -> Nothing
  Just _ -> Just $ Grid (Z.moveLeft <$> z)

safeRight :: Grid a -> Maybe (Grid a)
safeRight (Grid z) = case Z.safeRight $ Z.focus z of
  Nothing -> Nothing
  Just _ -> Just $ Grid (Z.moveRight <$> z)

update :: (a -> a) -> Grid a -> Grid a
update f (Grid z) = Grid $ Z.update (Z.update f) z

toLists :: Grid a -> [[a]]
toLists (Grid z) = Z.toList $ Z.toList <$> z

instance (Show a) => Show (Grid a) where
  -- show (Grid z) = intercalate "\n" (Z.toList $ show <$> z)
  show (Grid z) = intercalate "\n" $ case show <$> z of
    Zipper l m r -> reverse l <> [m <> " <"] <> r

grid = Grid $ duplicate (fromListInd [0 .. 5] 2)

instance Comonad Grid where
  extract (Grid z) = focus (focus z)
  duplicate (Grid z) = Grid <$> Grid layers
    where
      layer u = Zipper (lefts u) u (rights u)
      lefts z@(Zipper l _ _) = fst <$> zip (tail $ iterate (fmap Z.moveLeft) z) l
      rights z@(Zipper _ _ r) = fst <$> zip (tail $ iterate (fmap Z.moveRight) z) r
      layers = layer (layer z)
