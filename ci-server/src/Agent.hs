module Agent where

import qualified Codec.Serialise as Serialise
import Core
import qualified Network.HTTP.Simple as HTTP
import RIO
import qualified Runner

data Cmd
  = StartBuild BuildNumber Pipeline
  deriving (Show, Eq, Generic, Serialise.Serialise)

data Msg
  = LogCollected BuildNumber Log
  | BuildUpdated BuildNumber Build
  deriving (Show, Eq, Generic, Serialise.Serialise)

data Config = Config
  { endpoint :: String
  }
  deriving (Show, Eq)

run :: Config -> Runner.Service -> IO ()
run config runner = forever do
  endpoint <- HTTP.parseRequest config.endpoint

  let req =
        endpoint
          & HTTP.setRequestMethod "POST"
          & HTTP.setRequestPath "agent/pull"

  res <- HTTP.httpLBS req
  traceShowIO res 
  let cmd = Serialise.deserialise (HTTP.getResponseBody res) :: Maybe Cmd

  traverse_ (runCommand runner) cmd
  threadDelay (1000 * 1000 * 1)

runCommand :: Runner.Service -> Cmd -> IO ()
runCommand runner = \case
  StartBuild buildNum pipeline -> do
    build <- runner.prepareBuild pipeline
    let hooks =
          Runner.Hooks
            { Runner.logCollected = traceShowIO
            }

    void $ runner.runBuild hooks build

