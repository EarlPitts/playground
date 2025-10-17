{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module IRCServer (runTCPServer, irc) where

import Control.Concurrent
import Control.Concurrent.STM
import qualified Control.Exception as E
import Control.Monad
import Control.Monad.Loops
import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as C8
import Data.Foldable
import qualified Data.List.NonEmpty as NE
import Debug.Trace
import Network.Socket
import Network.Socket.ByteString (recv, sendAll)
import Text.Parsec
import Text.Parsec.String
import Prelude hiding (log)

type NickName = String

type UserName = String

type Host = String

type ServerName = String

type RealName = String

data User = User
  { userName :: UserName,
    nickName :: NickName,
    host :: Host,
    serverName :: ServerName,
    realName :: RealName,
    userSocket :: Socket
  }
  deriving (Show, Eq)

data ClientMsg
  = Unimplemented
  | Nick NickName
  | NewUser UserName Host ServerName RealName
  deriving (Show, Eq)

data ServerMsg
  = Welcome NickName UserName Host
  deriving (Eq)

instance Show ServerMsg where
  show (Welcome nickName userName hostName) =
    ":irc.localhost" -- TODO get this from the TCPServer
      <> " 001 "
      <> nickName 
      <> " :Welcome to the Internet Relay Network "
      <> nickName
      <> "!"
      <> userName
      <> "@"
      <> hostName
      <> "\r\n"

pMsgs :: Parser [ClientMsg]
pMsgs = pClientMsg `sepEndBy` crlf

pClientMsg :: Parser ClientMsg
pClientMsg = pUser <|> pNick <|> pUnimplemented

pUnimplemented :: Parser ClientMsg
pUnimplemented = manyTill anyChar crlf >> pure Unimplemented

pNick :: Parser ClientMsg
pNick = Nick <$> (string "NICK " *> many letter)

notSpace :: Parser Char
notSpace = noneOf [' ']

pUser :: Parser ClientMsg
pUser = do
  string "USER "
  user <- many notSpace
  space
  host <- many notSpace
  space
  server <- many notSpace
  space
  realName <- char ':' *> many (letter <|> char ' ')
  return $ NewUser user host server realName

type Server = User -> TVar [User] -> IO ()

debugMode :: Bool
debugMode = True

log :: String -> IO ()
log = when debugMode . putStrLn . ("[DEBUG] " <>)

mkMsg :: Int -> S.ByteString -> S.ByteString
mkMsg n msg = "Guest " <> C8.pack (show n) <> ": " <> msg

sendMessage :: Int -> S.ByteString -> Socket -> [Socket] -> IO ()
sendMessage n msg s socks =
  traverse_ (flip sendAll (mkMsg n msg)) (filter ((/=) s) socks)

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
          case parse pClientMsg "client msg" (C8.unpack msg) of
            Right Unimplemented -> go Nothing
            Right (Nick nickName) -> if isNickTaken nickName users then undefined >> return Nothing else go (Just nickName)
            Right (NewUser u h s r) -> return $ fmap (\n -> User u n h s r sock) nick
            Left err -> print err >> undefined

welcome :: User -> IO ()
welcome (User userName nickName hostName _ _ sock) = do
  sendAll sock $ C8.pack (show (Welcome userName nickName hostName))

irc :: Server
irc u us = do
  log $ "waiting for " <> nickName u <> "'s messages..."
  msg <- recv (userSocket u) 1024
  print msg
  log $ "message received from " <> nickName u <> ": " <> C8.unpack msg
  unless (S.null msg) $ do
    irc u us

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
        E.bracketOnError (accept sock) (close . fst) $ \(conn, _peer) -> do
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
              void $ forkFinally (server u users) (const $ gracefulClose conn 5000)
