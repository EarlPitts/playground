{-# LANGUAGE OverloadedStrings #-}

module Networking.ChatServer where

import Control.Concurrent
import Control.Concurrent.STM
import qualified Control.Exception as E
import Control.Monad
import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as C8
import Data.Foldable
import qualified Data.List.NonEmpty as NE
import Debug.Trace
import Network.Socket
import Network.Socket.ByteString (recv, sendAll)
import System.Console.ANSI
import Prelude hiding (log)

type Server = TVar [Socket] -> Socket -> Int -> IO ()

main :: IO ()
main = runTCPServer Nothing "3000" echo

debugMode :: Bool
debugMode = True

log :: String -> IO ()
log = when debugMode . putStrLn . ("[DEBUG] " <>)

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
    socks <- readTVarIO sockets
    log $ "got socket list" <> show socks
    sendMessage n msg s socks
    log "sent message to all sockets"
    echo sockets s n

runTCPServer :: Maybe HostName -> ServiceName -> Server -> IO a
runTCPServer mhost port server = do
  addr <- resolve
  sockets <- newTVarIO ([] :: [Socket])
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
          ss <- readTVarIO sockets
          traverse_ (flip sendAll ("Guest " <> C8.pack (show $ (length ss) + 1) <> " joined\n")) ss
          socketNum <- atomically $ do
            ss <- readTVar sockets
            writeTVar sockets (conn : ss)
            pure (length ss + 1)
          log "added socket to list"
          void $ forkFinally (server sockets conn socketNum) (const $ gracefulClose conn 5000)

-- Client stuff
setColor :: Int -> IO ()
setColor = setSGR . pickColor

pickColor :: Int -> [SGR]
pickColor n = [foreground i c]
  where
    foreground = SetColor Foreground
    (i, c) = allColors !! mod n 14

allColors :: [(ColorIntensity, Color)]
allColors = [(i, c) | i <- [Dull, Vivid], c <- cols]
  where
    cols = [Red, Green, Yellow, Blue, Magenta, Cyan, White]

resetColor :: IO ()
resetColor = setSGR [Reset]

