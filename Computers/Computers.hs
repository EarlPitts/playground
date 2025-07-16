import Control.Monad
import Control.Monad.State
import Data.List (intercalate)
import Data.Maybe
import Gates
import Nat
import Zipper
import Prelude hiding (and, not, or, read)

map' :: (Monad m) => (a -> b) -> m a -> m b
map' f ma = bind ma m1
  where
    bind ma f = (>=>) (const ma) f ()

    m1 a = return (f a)

f :: Int -> State Int Int
f _ = do
  x <- get
  return (x + 1)

g :: Int -> State Int Int
g y = do
  modify succ
  x <- get
  return (x + y)

h = f >=> g

type Tape = Zipper Nat

type Position = Nat

type Program a = State Tape a

tapeSize = 10
tape = fromList (replicate tapeSize Z) 0

moveLeft :: Program ()
moveLeft = modify $ fromJust . zipLeft

moveRight :: Program ()
moveRight = modify $ fromJust . zipRight

read :: Program Nat
read = gets zipGet

write :: Nat -> Program ()
write = modify . zipSet

incr :: Program ()
incr = modify (\t -> zipSet (S (zipGet t)) t)

prog = do
  incr
  incr
  incr
  incr
  moveRight
  incr
  incr
  n <- read
  moveRight
  moveRight
  write n

-- prog = replicateM_ 10 z
--

run :: Program a -> (a, [Int])
run p = (fmap . fmap) toInt $ toList <$> runState p tape

exec :: Program a -> [Int]
exec p = toInt <$> toList (execState p tape)

eval :: Program Nat -> Int
eval p = toInt (evalState p tape)
