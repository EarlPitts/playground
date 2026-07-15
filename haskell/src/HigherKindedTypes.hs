module HigherKindedTypes where

data NonEmpty f a = MkNonEmpty
  { head :: a
  , tail :: f a
  }
  deriving (Show)

x = MkNonEmpty 2 [2]
y = MkNonEmpty 2 (Just 2)
