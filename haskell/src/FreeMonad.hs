module FreeMonad where

import Control.Monad.Free

data Console a = PrintLine String a | GetLine (String -> a) deriving (Functor)

echo :: Console (Console ())
echo = GetLine (\line -> PrintLine line ())

readLine :: Free Console String
readLine = liftF (GetLine id)

printLine :: String -> Free Console ()
printLine line = liftF (PrintLine line ())

echo' :: Free Console ()
echo' = do
  line <- readLine
  printLine line

runConsole :: Free Console a -> IO a
runConsole =
  iterM
    ( \op -> case op of
        GetLine f -> getLine >>= f
        PrintLine line a -> putStrLn line >> a
    )

-- https://haskellforall.com/2012/06/you-could-have-invented-free-monads

data Toy b next
  = Output b next
  | Bell next
  | Done

-- data Fix f = Fix (f (Fix f))
data FixE f e = Fix (f (FixE f e)) | Throw e

data Err = Err

type Program = FixE (Toy String) Err

prog :: Program
prog = Fix (Bell (Fix (Output "kecske" (Fix (Output "sajt" (Fix Done))))))

run :: Program -> IO ()
run (Fix (Output out next)) = putStrLn out >> run next
run (Fix (Bell next)) = putStrLn "Ding-Dong" >> run next
run (Fix Done) = pure ()

catch :: (Functor f) => FixE f e1 -> (e1 -> FixE f e2) -> FixE f e2
catch (Fix p) f = Fix (fmap (flip catch f) p)
catch (Throw e) f = f e

instance Functor (Toy b) where
  fmap f (Output x next) = Output x (f next)
  fmap f (Bell next) = Bell (f next)
  fmap _ Done = Done

subroutine :: Program
subroutine = Fix (Output "One" (Fix (Output "Two" (Throw Err))))

prog2 = subroutine `catch` (\Err -> Fix (Bell (Fix Done)))

data Free' f r = Free' (f (Free' f r)) | Pure' r

instance (Functor f) => Functor (Free' f) where
  fmap f (Free' p) = Free' (fmap (fmap f) p)
  fmap f (Pure' r) = Pure' (f r)

instance (Functor f) => Applicative (Free' f) where
  pure = Pure'
  Free' p <*> x = Free' (fmap (<*> x) p)
  Pure' f <*> x = fmap f x

instance (Functor f) => Monad (Free' f) where
  (Free' p) >>= f = Free' (fmap (>>= f) p)
  (Pure' r) >>= f = f r

liftF' :: (Functor f) => f r -> Free' f r
liftF' command = Free' (fmap Pure' command)

output :: a -> Free' (Toy a) ()
output x = liftF' (Output x ())

bell :: Free' (Toy a) ()
bell = liftF' (Bell ())

done :: Free' (Toy a) r
done = liftF' Done

prog'' = do
  output "sajt"
  bell
  done

run' :: Free' (Toy String) r -> IO ()
run' (Free' (Output a r)) = putStrLn a >> run' r
run' (Free' (Bell r)) = putStrLn "Ding-Dong" >> run' r
run' (Free' Done) = pure ()
run' (Pure' e) = undefined
