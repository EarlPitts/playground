{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- https://blog.ocharles.org.uk/blog/posts/2016-01-26-transformers-free-monads-mtl-laws.html

module TransVsMTL where

import Control.Monad
import qualified Control.Monad.Reader as R
import qualified Control.Monad.State as ST
import Control.Monad.Trans
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State hiding (modify', state)
import Data.Maybe

data St = St Int

data Env = Env Int

-- Building it up ourselves
newtype M a = M (Env -> St -> IO (a, St))

-- We have a lot of boilerplate
instance Functor M where
  fmap f (M p) = M $ \e s -> do
    (a, s') <- p e s
    return (f a, s')

instance Applicative M where
  (<*>) (M f) (M a) = M $ \e s -> do
    (f', s') <- f e s
    (a', s'') <- a e s'
    return (f' a', s'')
  pure a = M (\e s -> pure (a, s))

instance Monad M where
  (>>=) (M a) f = M $ \e s -> do
    (a, s') <- a e s
    let (M b) = f a
    b e s'

ask' :: M Env
ask' = M $ \e s -> pure (e, s)

modify' :: (St -> St) -> M ()
modify' f = M $ \_ s -> pure ((), f s)

get' :: M St
get' = M $ \_ s -> pure (s, s)

program :: M Int
program = do
  (Env i) <- ask'
  modify' (\(St n) -> St (n + i))
  (St n) <- get'
  return n

runProgram :: (Show a) => M a -> IO a
runProgram (M f) = case f (Env 5) (St 3) of
  res -> fst <$> res

-- Using monad transformers from `transformers`
-- and deriving everything
-- Much easier to define
newtype M' a = M' (ReaderT Env (StateT St IO) a) deriving (Functor, Applicative, Monad)

-- Usage is not that ergonomic
-- We have to lift programs into the
-- right level of the monad stack
-- Reasoning is also harder, as we wrap
-- everything in the whole M' stack,
-- so we don't know which monad the
-- program actually uses
program' :: M' Int
program' = do
  (Env i) <- M' ask
  M' (lift (modify (\(St n) -> St (n + i))))
  (St n) <- M' (lift get)
  return n

runProgram' :: (Show a) => M' a -> IO a
runProgram' (M' p) = evalStateT (runReaderT p (Env 5)) (St 3)

-- Using MTL
newtype M'' a = M'' (R.ReaderT Env (ST.StateT St IO) a)
  deriving
    ( Functor,
      Applicative,
      Monad,
      (R.MonadReader Env),
      ST.MonadState St
    )

-- We can use the type classes from
-- `mtl` to get fine-grained control
-- over the effects
env :: (R.MonadReader Env m) => m Env
env = R.ask

state :: (ST.MonadState St m) => m St
state = ST.get

alter :: (ST.MonadState St m) => (St -> St) -> m ()
alter = ST.modify

-- We got rid of the lifting
program'' :: M'' Int
program'' = do
  (Env i) <- env
  alter (\(St n) -> St (n + i))
  (St n) <- state
  return n

runProgram'' :: (Show a) => M'' a -> IO a
runProgram'' (M'' p) = ST.evalStateT (R.runReaderT p (Env 5)) (St 3)
