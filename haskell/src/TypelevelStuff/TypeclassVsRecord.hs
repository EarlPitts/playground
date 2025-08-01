module TypelevelStuff.TypeclassVsRecord where

import Prelude hiding (Monoid)

class TClass a where
  f :: a -> Int -> Int
  g :: String -> String -> a

instance TClass Int where
  f a n = n
  g str str2 = 2

data Record a = Record
  { f' :: a -> Int -> Int,
    g' :: String -> String -> a
  }

r = Record {f' = \a n -> n, g' = \str str2 -> 2}

class HKTClass m where
  h :: Int -> m Int
  j :: String -> m String

instance HKTClass Maybe where
  h = Just
  j str = Nothing

data Record' m = Record'
  { h' :: Int -> m Int,
    j' :: String -> m String
  }

r' = Record' {h' = Just, j' = const Nothing}

-- Composition

data Monoid a = Monoid
  { z :: a,
    mapp :: a -> a -> a
  }

intMonoid = Monoid {z = 0, mapp = (+)}

-- You have to pass the monoid explicitly,
-- instead of using a constraint and letting
-- the compiler search for it
maybeMonoid :: Monoid a -> Monoid (Maybe a)
maybeMonoid ma =
  Monoid
    { z = Nothing,
      mapp = \m m' -> case (m, m') of
        (Just x, Just y) -> Just (mapp ma x y)
    }

-- Combining them is manual
maybeIntMonoid = maybeMonoid intMonoid

-- Using it
_ = z maybeIntMonoid
_ = mapp maybeIntMonoid (Just 1) (Just 2)
