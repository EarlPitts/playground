module Nat where

data Nat = S Nat | Z deriving (Show)

toInt :: Nat -> Int
toInt Z = 0
toInt (S n) = succ (toInt n)
