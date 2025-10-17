module Main (main) where

import IRCServer

main :: IO ()
main = runTCPServer Nothing "3000" irc
