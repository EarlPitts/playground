module FoldsLaziness where

-- https://wiki.haskell.org/index.php?title=Foldr_Foldl_Foldl%27

import Prelude hiding (fold', foldl, foldr)

veryBigList = [1 .. 10000000]

foldr _ z [] = z
foldr f z (x : xs) = f x (foldr f z xs)

sum1 = foldr (+) 0

-- This results in a stack overflow, because
-- (+) is strict in both its arguments

-- This should also blow the stack, but for me it works fine
-- (at least until I'm out of memory)
foldl _ z [] = z
foldl f z (x : xs) =
  let z' = f z x
   in foldl f z' xs

sum2 = foldl (+) 0

-- We force the reduction on z' first
foldl' _ z [] = z
foldl' f z (x : xs) =
  let z' = f z x
   in seq z' $ foldl' f z' xs

sum3 = foldl' (+) 0

-- foldr also short-circuits!
sum4 = foldr add 0

add 1000 _ = 0
add x y = x + y

-- foldl doesn't
sum5 = foldl add 0
