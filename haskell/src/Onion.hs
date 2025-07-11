{-# LANGUAGE OverloadedStrings #-}

module Onion where

import Control.Monad
import Control.Monad.Catch
import Control.Monad.Managed
import Data.ByteString.Char8
import Data.Foldable
import Data.Traversable
import Database.Redis as R
import System.IO hiding (withFile)

-- Infrastructure layer
redisResource :: Managed R.Connection
redisResource =
  let acquire = R.checkedConnect R.defaultConnectInfo
      release = R.disconnect
   in managed $ bracket acquire release

main = runManaged $ do
  conn <- redisResource
  liftIO $ runRedis conn $ do
    set "hello" "hello"
  return ()

-- Service layer
data Memoizer = Memoizer
  { memoize :: Int -> Int -> IO ()
  }

mkMemoizer :: R.Connection -> Memoizer
mkMemoizer c =
  Memoizer
    { memoize = memoize' c
    }

memoize' :: R.Connection -> Int -> Int -> IO ()
memoize' c input output = runRedis c $ do
  set (pack (show input)) (pack (show output))
  return ()
