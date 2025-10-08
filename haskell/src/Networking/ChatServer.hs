{-# LANGUAGE OverloadedStrings #-}

module Networking.ChatServer where

import Control.Concurrent
import qualified Control.Exception as E
import Control.Monad
import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as C8
import Data.Foldable
import qualified Data.List.NonEmpty as NE
import Debug.Trace
import Network.Socket
import Network.Socket.ByteString (recv, sendAll)
import Prelude hiding (log)

type Server = MVar [Socket] -> Socket -> Int -> IO ()

main :: IO ()
main = runTCPServer Nothing "3000" echo

debugMode :: Bool
debugMode = True

log :: String -> IO ()
log = when debugMode . print

mkMsg :: Int -> S.ByteString -> S.ByteString
mkMsg n msg = "Guest " <> C8.pack (show n) <> ": " <> msg

sendMessage :: Int -> S.ByteString -> Socket -> [Socket] -> IO ()
sendMessage n msg s socks =
  traverse_ (flip sendAll (mkMsg n msg)) (filter ((/=) s) socks)

echo :: Server
echo sockets s n = do
  log "waiting for message..."
  msg <- recv s 1024
  log $ "message received from socket " <> show s
  unless (S.null msg) $ do
    socks <- readMVar sockets
    log $ "got socket list" <> show socks
    sendMessage n msg s socks
    log "sent message to all sockets"
    echo sockets s n

runTCPServer :: Maybe HostName -> ServiceName -> Server -> IO a
runTCPServer mhost port server = do
  addr <- resolve
  sockets <- newMVar ([] :: [Socket])
  E.bracket (open addr) close (loop sockets)
  where
    resolve = do
      let hints =
            defaultHints
              { addrFlags = [AI_PASSIVE],
                addrSocketType = Stream
              }
      NE.head <$> (traceShowId <$> (getAddrInfo (Just hints) mhost (Just port)))
    open addr =
      E.bracketOnError (openSocket addr) close $ \sock -> do
        setSocketOption sock ReuseAddr 1
        withFdSocket sock setCloseOnExecIfNeeded
        bind sock $ addrAddress addr
        listen sock 1024
        return sock
    loop sockets sock =
      forever $
        E.bracketOnError (accept sock) (close . fst) $ \(conn, _peer) -> do
          log "accepted connection"
          modifyMVar_ sockets (\sockets -> pure $ conn : sockets)
          socketNum <- length <$> readMVar sockets
          log "added socket to list"
          void $ forkFinally (server sockets conn socketNum) (const $ gracefulClose conn 5000)
