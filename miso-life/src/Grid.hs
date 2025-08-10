module Grid where

import Control.Comonad
import Control.Monad
import Data.List
import Data.Maybe
import qualified Zipper as Z

type Row = Int
type Col = Int

newtype Grid a = Grid {runGrid :: Z.Zipper (Z.Zipper a)}
  deriving (Eq, Functor)

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

update' :: Row -> Col -> (a -> a) -> Grid a -> Maybe (Grid a)
update' r c f g = do
  row <- safeDownN r g
  col <- safeRightN c row
  modified <- Just $ update f col
  newCol <- safeLeftN c modified
  safeUpN r newCol

set :: a -> Grid a -> Grid a
set a g = update (const a) g

safeDownN :: Int -> Grid a -> Maybe (Grid a)
safeDownN 0 g = Just g
safeDownN n g = safeDown g >>= safeDownN (n - 1)

safeUpN :: Int -> Grid a -> Maybe (Grid a)
safeUpN 0 g = Just g
safeUpN n g = safeUp g >>= safeUpN (n - 1)

safeLeftN :: Int -> Grid a -> Maybe (Grid a)
safeLeftN 0 g = Just g
safeLeftN n g = safeLeft g >>= safeLeftN (n - 1)

safeRightN :: Int -> Grid a -> Maybe (Grid a)
safeRightN 0 g = Just g
safeRightN n g = safeRight g >>= safeRightN (n - 1)

point :: Int -> Int -> Grid a -> Maybe (Grid a)
point r c g = safeDownN r g >>= safeRightN c

toLists :: Grid a -> [[a]]
toLists (Grid z) = Z.toList $ Z.toList <$> z

fromLists :: [[a]] -> Grid a
fromLists = Grid . Z.fromList . fmap Z.fromList

focus :: Grid a -> a
focus = Z.focus . Z.focus . runGrid

adjacent :: Grid a -> [Grid a]
adjacent g = mapMaybe ($ g) [up, down, left, right, upRight, upLeft, downRight, downLeft]
  where
    up = safeUp
    down = safeDown
    left = safeLeft
    right = safeRight
    upRight = safeRight >=> safeUp
    upLeft = safeLeft >=> safeUp
    downRight = safeRight >=> safeDown
    downLeft = safeLeft >=> safeDown

instance (Show a) => Show (Grid a) where
  -- show (Grid z) = intercalate "\n" (Z.toList $ show <$> z)
  show (Grid z) = intercalate "\n" $ case show <$> z of
    Z.Zipper l m r -> reverse l <> [m <> " <"] <> r

instance Comonad Grid where
  extract = focus
  duplicate (Grid z) = Grid <$> Grid layers
    where
      layer u = Z.Zipper (lefts u) u (rights u)
      lefts z@(Z.Zipper l _ _) = fst <$> zip (tail $ iterate (fmap Z.moveLeft) z) l
      rights z@(Z.Zipper _ _ r) = fst <$> zip (tail $ iterate (fmap Z.moveRight) z) r
      layers = layer (layer z)
