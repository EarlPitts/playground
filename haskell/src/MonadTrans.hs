import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.Identity
import Control.Monad.Except

import Text.Read (readMaybe)
import Control.Monad.Trans.Maybe
import Control.Applicative

f :: StateT Int (ReaderT Int Identity) Int
f = do
  n <- get
  m <- lift ask
  put 123
  return $ n + m

ex1 = runReaderT (execStateT f 3) 2 -- This gives back 123 (the value you put in last)
ex2 = runReaderT (evalStateT f 3) 2 -- This gives back 5 (the sum of 3 from state and 2 from reader)

g :: StateT Int (ReaderT Int Maybe) Int
g = do
  n <- get
  m <- lift ask
  o <- (lift . lift) (Just 3)
  put 123
  return $ n + m + o

ex3 = runReaderT (evalStateT g 3) 2 -- Gives back 8 as expected

h :: IO ()
h = do
  let x = do
        y <- Just 3
        return y
  print x

readInt :: IO (Maybe Int)
readInt = do
    string <- getLine
    return (readMaybe string)

readThreeInts :: IO (Maybe (Int, Int, Int))
readThreeInts = runMaybeT $ do
  int0 <- MaybeT readInt
  int1 <- MaybeT readInt
  int2 <- MaybeT readInt
  return (int0, int1, int2)

ensureInt :: IO (Maybe Int)
ensureInt = do
    maybeInt <- readInt
    case maybeInt of
        Nothing -> ensureInt
        Just n  -> return (Just n)

ensureInt' :: IO (Maybe Int)
ensureInt' = runMaybeT loop
  where
    loop = MaybeT readInt <|> loop

example :: IdentityT IO ()
example = do
    lift (print 2)  -- Output: 2
    lift (print 3)  -- Output: 3

example' :: ReaderT Int (ReaderT Int IO) ()
example' = do
  a <- ask
  b <- lift ask
  liftIO $ print (a,b)

example1 :: WriterT [Char] IO ()
example1 = do
    tell "ab"
    lift (print 1)  -- Output: 1
    tell "c"

example2 :: StateT Int IO ()
example2 = do
  n <- get
  lift (print n)
  put (n+1)

main = do
  ((),n) <- runStateT example2 8
  print n

exceptExample :: ExceptT String IO ()
exceptExample = do
  lift (print 1)
  throwError "jaj!"
  lift (print 2)

examplee :: StateT Int (ExceptT String IO) ()
examplee = do
  n <- get
  
  if n < 0
    then throwError "Juj!"
    else liftIO (print "AAA!")

exampleee :: ExceptT String (StateT Int IO) ()
exampleee = do
  n <- get
  
  if n < 0
    then throwError "Juj!"
    else liftIO (print "AAA!")
