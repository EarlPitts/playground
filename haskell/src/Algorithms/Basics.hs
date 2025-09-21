module Algorithms.Basics where

import Data.List
import Prelude hiding (until)

h :: [a] -> a
h = foldr const (error "empty list")

inserts :: a -> [a] -> [[a]]
inserts a [] = [[a]]
inserts a (a' : as) = (a : a' : as) : map (a' :) (inserts a as)

select :: [a] -> [(a, [a])]
select [] = []
select (x : xs) = (x, xs) : [(y, x : ys) | (y, ys) <- select xs]

perms :: [a] -> [[a]]
perms [] = [[]]
perms (x : xs) = [zs | ys <- perms xs, zs <- inserts x ys]

perms' :: [a] -> [[a]]
perms' = foldr (concatMap . inserts) [[]]

perms'' :: [a] -> [[a]]
perms'' [] = [[]]
perms'' xs = [x : zs | (x, ys) <- select xs, zs <- perms'' ys]

perms''' :: [a] -> [[a]]
perms''' [] = [[]]
perms''' xs = concatMap subperms (select xs)
  where
    subperms (x, ys) = map (x :) (perms''' ys)

-- Recursive loops
until :: (a -> Bool) -> (a -> a) -> a -> a
until p f x = if p x then x else until p f (f x)

while :: (a -> Bool) -> (a -> a) -> a -> a
while p = until (not . p)

foldCat :: (a -> b -> b) -> b -> [[a]] -> b
foldCat f = foldr (flip (foldr f))

wrap x = x : []

unwrap [x] = x

single [x] = True
single _ = False

rev :: [a] -> [a]
rev = foldl (flip (:)) []

rev' :: [a] -> [a]
rev' as = go as []
  where
    go [] acc = acc
    go (a : as) acc = go as (a : acc)

map' :: (a -> b) -> [a] -> [b]
map' f = foldr ((:) . f) []

filter' :: (a -> Bool) -> [a] -> [a]
filter' p = foldr f' []
  where
    f' a as = if p a then a : as else as

foldFilt :: (a -> Bool) -> (a -> b -> b) -> b -> [a] -> b
foldFilt p f = foldr (\x y -> if p x then f x y else y)

takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' p = foldr (\x xs -> if p x then x : xs else []) []

dropWhileEnd' :: (a -> Bool) -> [a] -> [a]
dropWhileEnd' p = foldr f []
  where
    f x xs = if null xs && p x then xs else x : xs

primes n = foldl check [2] [3 .. n]
  where
    check ps n = if any (\p -> mod n p == 0) ps then ps else n : ps

integer :: [Int] -> Int
integer = foldl1 (\acc n -> n + acc * 10)

fraction :: [Int] -> Float
fraction = foldr f 0
  where
    f n acc = (acc + fromIntegral n) / 10.0
