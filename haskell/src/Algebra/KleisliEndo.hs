module Algebra.KleisliEndo where

import Control.Monad

newtype KleisliEndo m a = KleisliEndo {appKleisliEndo :: a -> m a}

instance (Monad m) => Semigroup (KleisliEndo m a) where
  f <> g = KleisliEndo (appKleisliEndo f >=> appKleisliEndo g)

instance (Monad m) => Monoid (KleisliEndo m a) where
  mempty = KleisliEndo pure
