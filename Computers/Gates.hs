module Gates where

import Prelude hiding (and, not, or)
import Control.Concurrent
import Control.Monad
import Control.Monad.State

data Wire = On | Off deriving (Eq, Show)

not :: Wire -> Wire
not On = Off
not Off = On

or :: Wire -> Wire -> Wire
or On On = On
or Off On = On
or On Off = On
or Off Off = Off

nor :: Wire -> Wire -> Wire
nor a b = not (or a b)

and :: Wire -> Wire -> Wire
and On On = On
and Off On = Off
and On Off = Off
and Off Off = Off

nand :: Wire -> Wire -> Wire
nand a b = not (and a b)

xor :: Wire -> Wire -> Wire
xor On On = Off
xor Off On = On
xor On Off = On
xor Off Off = Off

add1 :: Wire -> Wire -> Wire -> (Wire, Wire)
add1 a b cin = (sum, carry)
  where
    sum = xor (xor a b) cin
    carry = or (or (and a b) (and a cin)) (and b cin)

type Nybble = (Wire, Wire, Wire, Wire)

add4 :: Nybble -> Nybble -> Nybble
add4 (a1, a2, a3, a4) (b1, b2, b3, b4) = (s1, s2, s3, s4)
  where
    (s1, c1) = add1 a1 b1 Off
    (s2, c2) = add1 a2 b2 c1
    (s3, c3) = add1 a3 b3 c2
    (s4, c4) = add1 a4 b4 c3

fromNybble :: Nybble -> Int
fromNybble (w1, w2, w3, w4) = foldl (\c n -> n + c * 2) 0 nums
  where
    fromWire w = if w == On then 1 else 0
    nums = fromWire <$> [w4, w3, w2, w1]

x :: Nybble
x = (Off, On, Off, On)

y :: Nybble
y = (Off, On, Off, Off)

add :: [Wire] -> [Wire] -> [Wire]
add w1 w2 = snd (foldl f (Off, []) (zip w1 w2))
  where
    f (cin, ws) (w1, w2) = (cout, sum : ws)
      where
        (sum, cout) = add1 w1 w2 cin

clock :: IO Wire
clock = forever $ threadDelay 1000000 >> pure On

flip' :: State Wire Wire
flip' = do
  modify not
  get

hold :: Wire -> State Wire Wire
hold v = modify (or v) >> get
