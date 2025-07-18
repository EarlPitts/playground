module Algebra.BoolMonoids where

import Control.Applicative

-- We have to make wrapper types, as there should always
-- be only one canonical instance for each type
-- Haskell's nominal type system allows us to treat
-- this two types as separate semantic objects

-- newtype Any = Any Bool

newtype Any = Any { getAny :: Bool } deriving (Show, Eq)

instance Semigroup Any where
  (<>) (Any b) (Any b') = Any (b || b')

instance Monoid Any where
  mempty = Any False
  mappend = (<>)

-- newtype Any = Any Bool

newtype All = All { getAll :: Bool } deriving (Show, Eq)

instance Semigroup All where
  (<>) (All b) (All b') = All (b && b')

instance Monoid All where
  mempty = All True
  mappend = (<>)
