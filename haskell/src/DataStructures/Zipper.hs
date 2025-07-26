module DataStructures.Zipper where

import Control.Comonad
import Control.Monad
import Data.Char
import Data.Foldable
import Data.List

data Zipper a = Zipper [a] a [a] deriving (Functor)

fromList :: [a] -> Zipper a
fromList (x : xs) = Zipper [] x xs
fromList [] = error "empty list"

fromListInd :: [a] -> Int -> Zipper a
fromListInd l i = Zipper (reverse $ take i l) (l !! i) (drop (i + 1) l)

moveLeft :: Zipper a -> Zipper a
moveLeft (Zipper (l : ls) m r) = Zipper ls l (m : r)
moveLeft z@(Zipper [] _ _) = z

moveRight :: Zipper a -> Zipper a
moveRight (Zipper l m (r : rs)) = Zipper (m : l) r rs
moveRight z@(Zipper _ _ []) = z

safeLeft :: Zipper a -> Maybe (Zipper a)
safeLeft (Zipper (l : ls) m r) = Just $ Zipper ls l (m : r)
safeLeft (Zipper [] _ _) = Nothing

safeRight :: Zipper a -> Maybe (Zipper a)
safeRight (Zipper l m (r : rs)) = Just $ Zipper (m : l) r rs
safeRight (Zipper _ _ []) = Nothing

focus :: Zipper a -> a
focus (Zipper _ m _) = m

lefts :: Zipper a -> [Zipper a]
lefts z@(Zipper l _ _) =
  fst <$> zip (tail $ iterate moveLeft z) l

rights :: Zipper a -> [Zipper a]
rights z@(Zipper _ _ r) =
  fst <$> zip (tail $ iterate moveRight z) r

update :: (a -> a) -> Zipper a -> Zipper a
update f (Zipper l m r) = Zipper l (f m) r

toList :: Zipper a -> [a]
toList (Zipper l m r) = reverse l <> [m] <> r

instance (Show a) => Show (Zipper a) where
  show (Zipper l m r) =
    "("
      <> intercalate "," (show <$> reverse l)
      <> " ["
      <> show m
      <> "] "
      <> intercalate "," (show <$> r)
      <> ")"

-- extract   <-> pure
-- duplicate <-> join
-- extend    <-> bind

instance Comonad Zipper where
  extract = focus
  duplicate z = Zipper (lefts z) z (rights z)
