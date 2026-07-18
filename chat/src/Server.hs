module Server (main) where

import qualified Data.ByteString as BS
import Data.IORef
import Data.List (delete)
import qualified Data.List.NonEmpty as NE
import Network.Socket
import Network.Socket.ByteString
import Protolude

data User = User
  { uSock :: Socket
  , uName :: ByteString
  }
  deriving (Show, Eq)

main :: IO ()
main = do
  users <- newIORef []

  let hints = defaultHints{addrFlags = [AI_PASSIVE], addrSocketType = Stream}
  addr <- NE.head <$> getAddrInfo (Just hints) (Just "127.0.0.1") (Just "5000")
  sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
  bind sock (addrAddress addr)
  listen sock 1024

  putText "Started receiving connections"

  forever $ acceptConns sock users

acceptConns :: Socket -> IORef [User] -> IO ()
acceptConns sock usersRef = do
  (conn, _) <- accept sock
  userName <- recv conn 1024
  let user = User conn userName

  print "accepted connection"
  modifyIORef usersRef (user :)

  void $ async $ handleUser user usersRef

handleUser :: User -> IORef [User] -> IO ()
handleUser user@(User sock name) usersRef = do
  msg <- recv sock 1024
  if (BS.null msg)
    then modifyIORef usersRef (delete user)
    else do
      socks <- fmap uSock . filter (/= user) <$> readIORef usersRef
      traverse_ (flip sendAll $ name <> "|" <> msg) socks
      handleUser user usersRef
