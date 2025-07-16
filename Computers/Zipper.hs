module Zipper where

data Zipper a = Zipper [a] a [a]

instance Show a => Show (Zipper a) where
  show (Zipper l m r) = unwords $ show <$> [reverse l,[m],r]

zipGet :: Zipper a -> a
zipGet (Zipper _ a _) = a

zipSet :: a -> Zipper a -> Zipper a
zipSet a (Zipper l _ r) = Zipper l a r

zipLeft :: Zipper a -> Maybe (Zipper a)
zipLeft (Zipper [] _ _) = Nothing
zipLeft (Zipper (a : as) m r) = Just $ Zipper as a (m : r)

zipRight :: Zipper a -> Maybe (Zipper a)
zipRight (Zipper _ _ []) = Nothing
zipRight (Zipper l m (a : as)) = Just $ Zipper (m : l) a as

fromList :: [a] -> Int -> Zipper a
fromList as n = Zipper l m r
  where
    l = reverse (take n as)
    r = drop (n+1) as
    m = as !! n

toList :: Zipper a -> [a]
toList (Zipper l m r) = reverse l ++ [m] ++ r

