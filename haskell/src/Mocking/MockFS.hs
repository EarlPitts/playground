{-# LANGUAGE FlexibleInstances #-}

module Mocking.MockFS where

import qualified Prelude
import Prelude hiding(readFile)
import Control.Monad.State

-- class Monad m => FSMonad m where
--   readFile :: FilePath -> m String

newtype FSService m = FSService { readFile :: FilePath -> m String }

numCharactersInFile :: Monad m => FSService m -> FilePath -> m Int
numCharactersInFile fsReader fileName = do
  contents <- readFile fsReader fileName
  return (length contents)

-- instance FSMonad IO where
--   readFile = Prelude.readFile
realFS :: FSService IO
realFS = FSService Prelude.readFile

-- Represents a mock filesystem with a single file
data MockFS = SingleFile FilePath String

mockFS :: FSService (State MockFS)
mockFS = FSService (\pathRequested -> do
    (SingleFile pathExisting contents) <- get
    if pathExisting == pathRequested
      then return contents
      else error "file not found")

-- instance FSMonad (State MockFS) where
--   readFile pathRequested = do
--     (SingleFile pathExisting contents) <- get
--     if pathExisting == pathRequested
--       then return contents
--       else error "file not found"

testNumCharactersInFile :: Bool
testNumCharactersInFile =
  evalState
    (numCharactersInFile mockFS "test.txt")
    (SingleFile "test.txt" "hello world")
    == 11
