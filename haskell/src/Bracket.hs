{-# LANGUAGE OverloadedStrings #-}

module Bracket where

import Control.Monad
import Control.Monad.Catch
import Control.Monad.Managed
import Data.Foldable
import Database.Redis as R
import System.IO hiding (withFile)

-- Managing resources with bracket
withFile = bracket (openFile "src/Bracket.hs" ReadMode) hClose

p = withFile readLines >>= traverse_ putStrLn

readLines :: Handle -> IO [String]
readLines h =
  hIsEOF h >>= \eof ->
    if eof
      then pure []
      else (:) <$> hGetLine h <*> readLines h

both f = withFile $ \h1 -> withFile $ \h2 -> f h1 h2

p' = both $ \h1 h2 ->
  readLines h1
    >>= traverse_ putStrLn
    >> readLines h1
    >>= traverse_ putStrLn

-- Using the managed library for better composition

res1 :: Managed Handle
res1 = managed withFile

res2 :: Managed Handle
res2 = managed withFile

p'' = do
  h1 <- res1
  h2 <- res2
  liftIO $ readLines h1 >>= traverse_ putStrLn
  liftIO $ readLines h2 >>= traverse_ putStrLn

redisResource :: Managed R.Connection
redisResource =
  let acquire = R.checkedConnect R.defaultConnectInfo
      release = R.disconnect
   in managed $ bracket acquire release

p''' = runManaged $ do
  conn <- redisResource
  liftIO $ runRedis conn $ do
    set "hello" "hello"
  return ()
