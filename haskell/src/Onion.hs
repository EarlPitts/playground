{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE UndecidableInstances #-}

module Onion where

import Control.Monad
import Control.Monad.Catch
import Control.Monad.Managed
import qualified Data.ByteString.Char8 as BS
import qualified Database.Redis as R

-- Infrastructure layer
redisResource :: Managed R.Connection
redisResource =
  let acquire = R.checkedConnect R.defaultConnectInfo
      release = R.disconnect
   in managed $ bracket acquire release

main f n = runManaged $ do
  conn <- redisResource
  let store = mkRedis conn
  res <- liftIO $ evaluator store f n
  liftIO $ print res

-- Service layer
data KVStore k v = KVStore
  { put :: k -> v -> IO (),
    get :: k -> IO (Maybe v)
  }

class (Read a, Show a) => Serializable a where
  toBS :: a -> BS.ByteString
  fromBS :: BS.ByteString -> a

instance (Read a, Show a) => Serializable a where
  toBS = BS.pack . show
  fromBS = read . BS.unpack

mkRedis :: (Serializable k, Serializable v) => R.Connection -> KVStore k v
mkRedis c =
  KVStore
    { put = put' c,
      get = get' c
    }

put' :: (Serializable k, Serializable v) => R.Connection -> k -> v -> IO ()
put' c k v = R.runRedis c $ do
  R.set (toBS k) (toBS v)
  return ()

get' :: (Serializable k, Serializable v) => R.Connection -> k -> IO (Maybe v)
get' c k = R.runRedis c $ do
  v <- R.get (toBS k)
  case v of
    Left _ -> error "huh"
    Right resp -> pure $ fromBS <$> resp

-- Logic layer
evaluator :: KVStore Int Integer -> (Int -> Integer) -> Int -> IO Integer
evaluator store f n = do
  res <- get store n
  case res of
    Just res -> pure res
    Nothing -> let res = f n in put store n res >> pure res

fibo :: Int -> Integer
fibo n = fib !! n
  where
    fib = 0 : 1 : zipWith (+) fib (tail fib)
