module Alternative where

import Control.Monad

findAllMatching :: (a -> Bool) -> [[a]] -> [a]
findAllMatching pred xss = do
    xs <- xss
    y  <- xs
    guard (pred y)
    -- unless (pred y) mempty
    return y

f :: Num a => (a -> Bool) -> a -> Maybe a
f pred x = do
    -- x  <- Nothing
    y  <- Just x
    guard (pred y)
    -- unless (pred y) Nothing
    return y
