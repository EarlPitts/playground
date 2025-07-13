{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}

module OnionWeb where

-- No this has nothing to do with tor

import Control.Concurrent
import Control.Monad
import Control.Monad.Catch
import Control.Monad.Managed
import qualified Data.ByteString.Char8 as BS
import Data.Functor.Identity
import qualified Data.Map as M
import qualified Data.Text.Lazy as T
import qualified Database.Redis as R
import qualified Web.Scotty as S

-- Infrastructure layer
redisResource :: Managed R.Connection
redisResource =
  let acquire =
        R.checkedConnect R.defaultConnectInfo
          >>= (\conn -> putStrLn "Redis connection opened" >> pure conn)
      release conn = R.disconnect conn >> putStrLn "Redis connection closed"
   in managed $ bracket acquire release

main = with redisResource $ \conn -> do
  let store = mkRedis conn
  S.scotty 3000 (routes store)

routes store = do
  S.get "/sajt" $ error "whoops"
  S.get "/:num" $ do
    n <- read <$> S.pathParam "num"
    res <- liftIO $ evaluator store fibo n
    S.html $ mconcat ["<h1>Fibo of ", T.pack (show n), " is ", T.pack (show res), "</h1>"]

-- Service layer
data KVStore k v m = KVStore
  { put :: k -> v -> m (),
    get :: k -> m (Maybe v)
  }

class (Read a, Show a) => Serializable a where
  toBS :: a -> BS.ByteString
  fromBS :: BS.ByteString -> a

instance (Read a, Show a) => Serializable a where
  toBS = BS.pack . show
  fromBS = read . BS.unpack

mkRedis :: (Serializable k, Serializable v) => R.Connection -> KVStore k v IO
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
evaluator :: (Monad m) => KVStore Int Integer m -> (Int -> Integer) -> Int -> m Integer
evaluator store f n = do
  res <- get store n
  case res of
    Just res -> pure res
    Nothing -> let res = f n in put store n res >> pure res

fibo :: Int -> Integer
fibo n = fib !! n
  where
    fib = 0 : 1 : zipWith (+) fib (tail fib)

stubKV :: KVStore Int Integer Identity
stubKV =
  KVStore
    { get = const $ pure Nothing,
      put = \_ _ -> pure ()
    }

type InMemStore = MVar (M.Map Int Integer)

fakeKV :: InMemStore -> KVStore Int Integer IO
fakeKV mvar =
  KVStore
    { get = fakeGet mvar,
      put = fakePut mvar
    }

fakeGet :: InMemStore -> Int -> IO (Maybe Integer)
fakeGet mvar n = do
  map <- liftIO $ readMVar mvar
  return $ M.lookup n map

fakePut :: InMemStore -> Int -> Integer -> IO ()
fakePut mvar n m = do
  modifyMVar_ mvar (pure . M.insert n m)

testMain f n = do
  mvar <- newMVar M.empty
  let store = fakeKV mvar
  evaluator store f n
  evaluator store f n
