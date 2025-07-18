-- https://lexi-lambda.github.io/blog/2017/06/29/unit-testing-effectful-haskell-with-monad-mock/

module Mocking.Fake where

{-# LANGUAGE GeneralizedNewtypeDeriving #-}

import Control.Arrow
import Control.Monad.Identity
import Control.Monad.State
import Data.Function
import Data.List
import Mocking.UnitTesting

newtype FakeFileSystemT m a = FakeFileSystemT (StateT [(FilePath, String)] m a)
  deriving (Functor, Applicative, Monad)

instance (MonadFail m) => MonadFS (FakeFileSystemT m) where
  readFile path = FakeFileSystemT $ get >>= \fs -> lookup path fs & maybe (fail $ "readFile: no such file '" ++ path ++ "'") return
  writeFile path contents = FakeFileSystemT . modify $ \fs -> (path, contents) : filter ((/= path) . fst) fs

fakeFileSystemT :: (Monad m) => [(FilePath, String)] -> FakeFileSystemT m a -> m (a, [(FilePath, String)])
fakeFileSystemT fs (FakeFileSystemT x) = second sort <$> runStateT x fs

f :: FakeFileSystemT Maybe ()
f = do
  UnitTesting.writeFile "sajt" "ezegysajt"
  content <- UnitTesting.readFile "sajt"
  UnitTesting.writeFile "lo" content
