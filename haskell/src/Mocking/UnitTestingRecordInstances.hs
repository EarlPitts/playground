module Mocking.UnitTestingRecordInstances where

import Control.Monad.Identity
import Control.Monad.Reader
import Control.Monad.Writer
import UnitTesting
import Prelude hiding (readFile, writeFile)

data MonadFSInst m = MonadFSInst
  { _readFile :: FilePath -> m String,
    _writeFile :: FilePath -> String -> m ()
  }

contentInst :: (MonadWriter [String] m) => MonadFSInst m
contentInst =
  MonadFSInst
    { _readFile = \_ -> return "hello",
      _writeFile = \_ contents -> tell [contents]
    }

pathInst :: (MonadWriter [FilePath] m) => MonadFSInst m
pathInst =
  MonadFSInst
    { _readFile = \path -> tell [path] >> return "",
      _writeFile = \path _ -> tell [path]
    }

newtype TestM log a
  = TestM (ReaderT (MonadFSInst (TestM log)) (Writer log) a)
  deriving
    ( Functor,
      Applicative,
      Monad,
      MonadReader (MonadFSInst (TestM log)),
      MonadWriter log
    )

logTestM :: MonadFSInst (TestM log) -> TestM log a -> log
logTestM inst (TestM m) = execWriter (runReaderT m inst)

instance (Monoid log) => MonadFS (TestM log) where
  readFile path = do
    f <- asks _readFile
    f path
  writeFile path contents = do
    f <- asks _writeFile
    f path contents

reverseContentTest = logTestM contentInst (reverseFile "sajt.txt")

reversePathTest = logTestM pathInst (reverseFile "sajt.txt")

-- We can create base instance that
-- has unimplemented methods, and
-- override the needed ones for test cases

-- This also helps in figuring out which
-- instance methods have to be stubbed
-- for each test case, as it will throw an
-- error if an unimplemented one is called

baseInst :: MonadFSInst m
baseInst =
  MonadFSInst
    { _readFile = error "unimplemented method '_readFile'",
      _writeFile = error "unimplemented method '_writeFile'"
    }

myInst = baseInst {_readFile = \_ -> Identity "sajt"}
