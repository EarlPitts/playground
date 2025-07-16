-- type IntSet = Int -> Bool

data Set = Set
  { isEmpty :: Bool,
    contains :: Int -> Bool,
    insert :: Int -> Set,
    union :: Set -> Set
  }

empty = this
  where
    this =
      Set
        { isEmpty = True,
          contains = const False,
          insert = insertObj this,
          union = id
        }

insertObj s n = if contains s n then s else this
  where
    this =
      Set
        { isEmpty = False,
          contains = \i -> i == n || contains s i,
          insert = insertObj this,
          union = \s -> unionObj this s
        }

unionObj s1 s2 = this
  where
    this =
      Set
        { isEmpty = isEmpty s1 && isEmpty s2,
          contains = \i -> contains s1 i || contains s2 i,
          insert = \i -> insertObj this i,
          union = \s -> unionObj this s
        }

evenObj = this
  where
    this =
      Set
        { isEmpty = False,
          contains = even,
          insert = insertObj this,
          union = unionObj this
        }

fullObj = this
  where
    this =
      Set
        { isEmpty = False,
          contains = const True,
          insert = const this,
          union = const this
        }

fromList :: [Int] -> Set
fromList = foldl insert empty

s1 = insert (insert (insert empty 2) 3) 4

s2 = insert (insert (insert empty 2) 8) 4

s3 = union s1 s2

showSet :: Set -> String
showSet s = show $ filter (contains s) [0 .. 10]
