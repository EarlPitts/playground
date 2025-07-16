data Bin = B | I Bin | O Bin deriving (Show, Eq)

data Nat = Zero | Suc Nat deriving (Show, Eq)

b = I (O (I B))

inc :: Bin -> Bin
inc B = I B
inc (I b) = O (inc b)
inc (O b) = I b

to :: Nat -> Bin
to Zero = O B
to (Suc n) = inc (to n)

plus :: Nat -> Nat -> Nat
plus Zero m = m
plus (Suc n) m = Suc (plus n m)

prod :: Nat -> Nat -> Nat
prod Zero _ = Zero
prod (Suc n) m = plus m (prod n m)

from :: Bin -> Nat
from B = Zero
from (I b) = plus (Suc Zero) (prod (Suc (Suc Zero)) (from b))
from (O b) = prod (Suc (Suc Zero)) (from b)
