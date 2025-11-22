module Agent where

import qualified Codec.Serialise as Serialise
import Core
import qualified Network.HTTP.Simple as HTTP
import RIO
import qualified Runner
import qualified System.Log.Logger as Logger

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
run config runner =
  forever do
    endpoint <- HTTP.parseRequest config.endpoint

    let req =
          endpoint
            & HTTP.setRequestMethod "POST"
            & HTTP.setRequestPath "/agent/pull"

    do
      res <- HTTP.httpLBS req
      let cmd = Serialise.deserialise (HTTP.getResponseBody res) :: Maybe Cmd

      traverse_ (runCommand config runner) cmd
      `catch` \e -> do
        Logger.warningM "ci.agent" "Server offline, waiting..."
        Logger.warningM "ci.agent" $ show (e :: HTTP.HttpException)

    threadDelay (1000 * 1000 * 1)

runCommand :: Config -> Runner.Service -> Cmd -> IO ()
runCommand config runner = \case
  StartBuild buildNum pipeline -> do
    let hooks =
          Runner.Hooks
            { Runner.logCollected = \log -> do
                traceShowIO log
                sendMessage config $ LogCollected buildNum log,
              Runner.buildUpdated = \build -> do
                traceShowIO build
                sendMessage config $ BuildUpdated buildNum build
            }

    let n = Core.displayBuildNumber buildNum
    Logger.infoM "ci.agent" $ "Starting build " <> n

    build <- runner.prepareBuild pipeline
    void $ runner.runBuild hooks build

    Logger.infoM "ci.agent" $ "Finished build " <> n

sendMessage :: Config -> Msg -> IO ()
sendMessage config msg = do
  endpoint <- HTTP.parseRequest config.endpoint

  let body = Serialise.serialise msg

  let req =
        endpoint
          & HTTP.setRequestMethod "POST"
          & HTTP.setRequestPath "/agent/send"
          & HTTP.setRequestBodyLBS body

  void $ HTTP.httpBS req
