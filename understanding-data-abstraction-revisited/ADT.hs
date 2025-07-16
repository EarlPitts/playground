import Data.List (intercalate)

data Set a = Empty | P a (Set a)

instance (Show a) => Show (Set a) where
  show s = "{" ++ intercalate "," (show <$> toList s) ++ "}"

-- data Set a where
--   Empty :: (Eq a) => Set a
--   P :: (Eq a) => a -> Set a -> Set a

mkEmpty :: (Eq a) => Set a
mkEmpty = Empty

insert :: (Eq a) => Set a -> a -> Set a
insert Empty elem = P elem Empty
insert s elem = if contains s elem then s else P elem s

fromList :: (Eq a) => [a] -> Set a
fromList = foldr (flip insert) Empty

toList :: Set a -> [a]
toList Empty = []
toList (P a as) = a : toList as

contains :: (Eq a) => Set a -> a -> Bool
contains (P a s) elem = a == elem || contains s elem
contains Empty _ = False
