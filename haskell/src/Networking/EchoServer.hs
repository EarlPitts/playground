module Networking.EchoServer where

import Control.Concurrent
import qualified Control.Exception as E
import Control.Monad
import qualified Data.ByteString as S
import qualified Data.List.NonEmpty as NE
import Debug.Trace
import Network.Socket
import Network.Socket.ByteString (recv, sendAll)

main :: IO ()
main = runTCPServer Nothing "3000" echo

echo :: Socket -> IO ()
echo s = do
  msg <- recv s 1024
  unless (S.null msg) $ do
    sendAll s msg
    echo s

runTCPServer :: Maybe HostName -> ServiceName -> (Socket -> IO a) -> IO a
runTCPServer mhost port server = do
  addr <- resolve
  E.bracket (open addr) close loop
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
    loop sock =
      forever $
        E.bracketOnError (accept sock) (close . fst) $ \(conn, _peer) ->
          void $ forkFinally (server conn) (const $ gracefulClose conn 5000)
