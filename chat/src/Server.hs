module Server where

import qualified Data.ByteString as BS
import Data.IORef
import qualified Data.List.NonEmpty as NE
import Network.Socket
import Network.Socket.ByteString
import Protolude
import Data.List (delete)

main :: IO ()
main = do
  socks <- newIORef []

  let hints = defaultHints{addrFlags = [AI_PASSIVE], addrSocketType = Stream}
  addr <- NE.head <$> getAddrInfo (Just hints) (Just "127.0.0.1") (Just "5000")
  sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
  bind sock (addrAddress addr)
  listen sock 1024

  putText "Started receiving connections"

  forever $ acceptConns sock socks

acceptConns :: Socket -> IORef [Socket] -> IO ()
acceptConns sock socksRef = do
  (conn, _) <- accept sock
  print "accepted connection"
  modifyIORef socksRef (conn :)
  void $ async $ handleUser conn socksRef

handleUser :: Socket -> IORef [Socket] -> IO ()
handleUser sock socksRef = do
  msg <- recv sock 1024
  if (BS.null msg)
    then modifyIORef socksRef (delete sock)
    else do
      socks <- filter (/= sock) <$> readIORef socksRef
      traverse_ (flip send $ msg) socks
      handleUser sock socksRef
