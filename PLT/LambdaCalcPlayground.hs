module LambdaCalcPlayground where

import Prelude hiding (and, or, fst, snd)

-- ### Booleans ###
tru x y = x
fls x y = y

test b x y = b x y

and x y = x y fls
or  x y = x tru y

toBool b = b True False

-- ### Pairs ###
pair f s b = b f s
fst p = p tru
snd p = p fls

-- ### Church numerals ###
c0 s z = z
c1 s z = s z
c2 s z = s (s z)
c3 s z = s (s (s z))

scc n s z = s (n s z)

plus m n s z = m s (n s z)
times m n = m (plus n) c0
power m n = m (times n) c1

iszro m = m (\_ -> fls) tru

-- Predecessor
zz = pair c0 c0
ss p = pair (snd p) (plus c1 (snd p))
prd m = fst (m ss zz)

minus m n = n prd m

equal m n = iszro (minus m n)

toNum n = n (succ :: Int -> Int) 0

-- TODO: List (cons, nil) with head, tail and isnil
