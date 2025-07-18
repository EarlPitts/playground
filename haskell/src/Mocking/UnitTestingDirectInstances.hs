module Mocking.UnitTestingDirectInstances where

import Mocking.UnitTesting
import Control.Monad.Writer
import Control.Monad.Identity
import Control.Monad

-- For testing stuff with return values
-- type TestM = Identity
newtype TestM a = TestM (Identity a)
  deriving (Show, Functor, Applicative, Monad)

-- Testing for side effects ("logging" it for comparison)
-- type TestLog = Writer [String]
newtype TestM' a = TestM' (Writer [String] a)
  deriving (Functor, Applicative, Monad, MonadWriter [String])

logTestM :: TestM' a -> [String]
logTestM (TestM' w) = execWriter w

instance MonadFS TestM where
  readFile _ = return "hello"
  writeFile _ _ = return ()

instance MonadFS TestM' where
  readFile _ = return "hello"
  writeFile _ content = void $ tell [content]

-- This won't work, reverse file has only
-- a unit as the return value
-- reverseTest :: FilePath -> TestM ()
-- reverseTest = reverseFile

-- This works, we are logging the
-- content that's "written"
reverseTest :: FilePath -> TestM' ()
reverseTest = reverseFile

-- Problem: we are not testing the
-- filepath part of the program

-- We can create a new type and
-- a new instance for it, but
-- it's a bunch of boilerplate

newtype TestM'' a = TestM'' (Writer [String] a)
  deriving (Functor, Applicative, Monad, MonadWriter [String])

instance MonadFS TestM'' where
  readFile path = tell [path] >> return ""
  writeFile path _ = tell [path]

logTestM' :: TestM'' a -> [String]
logTestM' (TestM'' w) = execWriter w

reverseTest' :: FilePath -> TestM'' ()
reverseTest' = reverseFile
