{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module IRCServer (runTCPServer, irc) where

import ClientMsg
import Control.Concurrent
import Control.Concurrent.STM
import qualified Control.Exception as E
import Control.Monad
import Control.Monad.Except
import Control.Monad.Identity
import Control.Monad.State
import Control.Monad.Trans.Maybe
import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as C8
import Data.Foldable
import qualified Data.List.NonEmpty as NE
import Debug.Trace
import Network.Socket
import Network.Socket.ByteString (recv, sendAll)
import ServerMsg
import Types
import UserService
import Prelude hiding (log)

type Server = User -> UserService -> IO ()

debugMode :: Bool
debugMode = True

rightToMaybe :: Either a b -> Maybe b
rightToMaybe (Right b) = Just b
rightToMaybe (Left _) = Nothing

irc :: Server
irc u userService = do
  log $ "waiting for " <> nickName u <> "'s messages..."
  msg <- recv (userSocket u) 1024
  log $ "message received from " <> nickName u <> ": " <> (show msg)
  unless (S.null msg) $ do
    case getClientMsg msg of
      Left err -> log (show msg) >> log (show err) >> irc u userService
      Right Quit -> log "removing user" >> removeUser userService (nickName u)
      Right cMsg -> do
        sendAll (userSocket u) (C8.pack $ show $ mkReply cMsg)
        irc u userService

log :: String -> IO ()
log = when debugMode . putStrLn . ("[DEBUG] " <>)

acceptUser :: UserService -> Socket -> IO (Maybe User) -- TODO error handling
acceptUser userService sock = go Nothing
  where
    go nick = do
      msg <- recv sock 1024
      log (show msg)
      if S.null msg
        then return Nothing
        else do
          case getClientMsg msg of
            Right (Nick n) -> do
              u <- runMaybeT $ findUser userService n
              case u of
                Nothing -> go (Just n)
                Just _ -> undefined
            Right (NewUser u h s r) ->
              traverse
                ( \n -> do
                    (Right result) <- runExceptT $ addUser userService (User u n h s r sock) -- TODO handle error
                    return result
                )
                nick
            Right _ -> undefined
            Left err -> log (show msg) >> log (show err) >> go Nothing

welcome :: User -> IO ()
welcome (User userName nickName hostName _ _ sock) = do
  sendAll sock $ C8.pack (show (Welcome userName nickName hostName))

mkReply :: ClientMsg -> ServerMsg
mkReply (Ping _) = Pong
mkReply (Join _) = undefined

handleNewConnection :: UserService -> Socket -> Server -> IO ()
handleNewConnection userService conn server = do
  log "accepted connection"
  log "read users"
  newUser <- acceptUser userService conn
  log "new user accepted"
  log (show newUser)
  case newUser of
    Nothing -> pure ()
    Just u -> do
      welcome u
      _ <- runExceptT $ addUser userService u -- TODO handle error
      log "added user to list"
      log "current users: " >> log . show <$> getUsers userService
      void $ forkFinally (server u userService) (const $ (log "closing connection" >> gracefulClose conn 5000))

runTCPServer :: Maybe HostName -> ServiceName -> Server -> IO a
runTCPServer mhost port server = do
  addr <- resolve
  users <- newTVarIO ([] :: [User])
  let storage = mkUserStorage users
  let userService = mkUserService storage
  E.bracket (open addr) close (loop userService)
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
    loop userService sock =
      forever $
        E.bracketOnError (accept sock) (close . fst) $
          \(conn, _peer) -> handleNewConnection userService conn server
