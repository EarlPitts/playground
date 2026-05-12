module Streaming where

import Conduit
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import Database.Redis as R

scanSource :: Connection -> ConduitT () BS.ByteString IO ()
scanSource conn = go cursor0
 where
  go cur = do
    result <- liftIO $ runRedis conn (scan cur)
    case result of
      Left _ -> pure ()
      Right (nextCur, keys) -> do
        yieldMany keys
        liftIO $ print keys
        if nextCur == cursor0
          then pure ()
          else go nextCur

kvPairs = liftA2 (,) ks ["a"]
 where
  ks = BS8.pack . show <$> [1 .. 1000]

setTTLs :: Connection -> ConduitT BS8.ByteString Void IO ()
setTTLs conn =
  mapM_C
    ( \key -> liftIO $ do
        result <- runRedis conn (expire key 20)
        print (key, result)
    )

main :: IO ()
main = do
  conn <- R.connect defaultConnectInfo
  _ <- runRedis conn $ mset kvPairs
  Conduit.connect
    (scanSource conn)
    (setTTLs conn)
