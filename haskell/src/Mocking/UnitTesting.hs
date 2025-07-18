-- https://lexi-lambda.github.io/blog/2016/10/03/using-types-to-unit-test-in-haskell/

module Mocking.UnitTesting where

import Control.Monad.Identity
import Control.Monad.Writer

class (Monad m) => MonadFS m where
  readFile :: FilePath -> m String
  writeFile :: FilePath -> String -> m ()

instance MonadFS IO where
  readFile = Prelude.readFile
  writeFile = Prelude.writeFile

reverseFile :: (MonadFS m) => FilePath -> m ()
reverseFile path = do
  contents <- Mocking.UnitTesting.readFile path
  Mocking.UnitTesting.writeFile path (reverse contents)

reverseIO :: FilePath -> IO ()
reverseIO = reverseFile
