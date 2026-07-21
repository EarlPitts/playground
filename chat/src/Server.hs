module Server (main) where

import Codec.Serialise
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.IORef
import Data.List (delete)
import qualified Data.List.NonEmpty as NE
import Network.Socket
import Network.Socket.ByteString
import Protolude

import Core

data User = User
  { uSock :: Socket
  , uName :: ByteString
  }
  deriving (Show, Eq)

withSocket :: AddrInfo -> (Socket -> IO a) -> IO a
withSocket addr = bracket (new addr) close

new :: AddrInfo -> IO Socket
new addr = do
  sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
  setSocketOption sock ReuseAddr 1
  bind sock (addrAddress addr)
  listen sock 1024
  pure sock

main :: IO ()
main = do
  users <- newIORef []

  let hints =
        defaultHints
          { addrFlags = [AI_PASSIVE]
          , addrSocketType = Stream
          }
  addr <-
    NE.head
      <$> getAddrInfo
        (Just hints)
        (Just (toS $ cHost defaultConfig))
        (Just (toS $ cPort defaultConfig))

  withSocket addr $ \sock -> do
    putText "Started receiving connections"
    forever $ acceptConns sock users

acceptConns :: Socket -> IORef [User] -> IO ()
acceptConns sock usersRef = do
  (conn, _) <- accept sock
  userName <- recv conn 1024
  let user = User conn userName

  putText $ "accepted connection from: " <> show userName
  modifyIORef usersRef (user :)

  systemMessage usersRef (userName <> " connected")

  void $ forkIO $ handleUser user usersRef

handleUser :: User -> IORef [User] -> IO ()
handleUser user@(User sock name) usersRef = do
  msg <- recv sock 1024
  if (BS.null msg)
    then do
      systemMessage usersRef (name <> " disconnected")
      modifyIORef usersRef (delete user)
      close sock
    else do
      sendMessage user usersRef msg
      handleUser user usersRef

sendMessage :: User -> IORef [User] -> ByteString -> IO ()
sendMessage user usersRef msg = do
  socks <- fmap uSock . filter (/= user) <$> readIORef usersRef
  let message = ServerMessage (uName user) msg
  traverse_ (flip sendAll $ LBS.toStrict (serialise message)) socks

systemMessage :: IORef [User] -> ByteString -> IO ()
systemMessage usersRef msg = do
  socks <- fmap uSock <$> readIORef usersRef
  let message = ServerMessage "system" msg
  traverse_ (flip sendAll $ LBS.toStrict (serialise message)) socks
