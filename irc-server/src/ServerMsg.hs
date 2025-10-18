module ServerMsg where

import Types

data ServerMsg
  = Welcome NickName UserName Host
  | Pong
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
  show Pong = ":irc.localhost PONG :irc.localhost\r\n"

