module TypelevelStuff.OpaqueTypes where

-- Just don't export this
newtype NonZeroInt = NonZeroInt Int deriving Show

mkNonZeroInt :: Int -> Maybe NonZeroInt
mkNonZeroInt 0 = Nothing
mkNonZeroInt n = Just (NonZeroInt n)

a = NonZeroInt 0
b = mkNonZeroInt 0

safeDiv :: Int -> NonZeroInt -> Int
safeDiv x (NonZeroInt y) = x `div` y

x  = liftA2 safeDiv (pure 4) (mkNonZeroInt 0) -- Nothing
x' = liftA2 safeDiv (pure 4) (mkNonZeroInt 2) -- Just 2
