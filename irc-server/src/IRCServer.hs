{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module IRCServer (runTCPServer, irc) where

import ClientMsg
import Control.Concurrent
import Control.Concurrent.STM
import qualified Control.Exception as E
import Control.Monad
import Control.Monad.Loops
import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as C8
import Data.Foldable
import Data.List
import qualified Data.List.NonEmpty as NE
import Debug.Trace
import Network.Socket
import Network.Socket.ByteString (recv, sendAll)
import ServerMsg
import Types
import Prelude hiding (log)

data User = User
  { userName :: UserName,
    nickName :: NickName,
    host :: Host,
    serverName :: ServerName,
    realName :: RealName,
    userSocket :: Socket
  }
  deriving (Show, Eq)

type Server = User -> TVar [User] -> IO ()

debugMode :: Bool
debugMode = True

irc :: Server
irc u us = do
  log $ "waiting for " <> nickName u <> "'s messages..."
  msg <- recv (userSocket u) 1024
  log $ "message received from " <> nickName u <> ": " <> (show msg)
  unless (S.null msg) $ do
    case getClientMsg msg of
      Left err -> log (show msg) >> log (show err) >> irc u us
      Right Quit -> log "removing user" >> (atomically $ removeUser u us)
      Right cMsg -> do
        sendAll (userSocket u) (C8.pack $ show $ mkReply cMsg)
        irc u us

log :: String -> IO ()
log = when debugMode . putStrLn . ("[DEBUG] " <>)

isNickTaken :: NickName -> [User] -> Bool
isNickTaken nick users = nick `elem` (nickName <$> users)

acceptUser :: Socket -> [User] -> IO (Maybe User)
acceptUser sock users = go Nothing
  where
    go nick = do
      msg <- recv sock 1024
      log (show msg)
      if S.null msg
        then return Nothing
        else do
          case getClientMsg msg of
            Right (Nick nickName) -> if isNickTaken nickName users then undefined >> return Nothing else go (Just nickName)
            Right (NewUser u h s r) -> return $ fmap (\n -> User u n h s r sock) nick
            Left err -> log (show msg) >> log (show err) >> go Nothing

welcome :: User -> IO ()
welcome (User userName nickName hostName _ _ sock) = do
  sendAll sock $ C8.pack (show (Welcome userName nickName hostName))

mkReply :: ClientMsg -> ServerMsg
mkReply (Ping _) = Pong
mkReply (Join _) = undefined

removeUser :: User -> TVar [User] -> STM ()
removeUser u us = modifyTVar us (\users -> delete u users)

handleNewConnection :: Socket -> TVar [User] -> Server -> IO ()
handleNewConnection conn users server = do
  log "accepted connection"
  us <- readTVarIO users
  log "read users"
  newUser <- acceptUser conn us
  log "new user accepted"
  log (show newUser)
  case newUser of
    Nothing -> pure ()
    Just u -> do
      welcome u
      atomically $ writeTVar users (u : us)
      log "added user to list"
      log $ "current users: " <> (show $ u : us)
      void $ forkFinally (server u users) (const $ (log "closing connection" >> gracefulClose conn 5000))


runTCPServer :: Maybe HostName -> ServiceName -> Server -> IO a
runTCPServer mhost port server = do
  addr <- resolve
  users <- newTVarIO ([] :: [User])
  E.bracket (open addr) close (loop users)
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
    loop users sock =
      forever $
        E.bracketOnError (accept sock) (close . fst) $
          \(conn, _peer) -> handleNewConnection conn users server
