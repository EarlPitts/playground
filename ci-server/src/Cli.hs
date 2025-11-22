module Cli where

import qualified Agent
import qualified Docker
import qualified JobHandler.Memory
import RIO
import qualified Runner
import qualified Server
import System.Log.Logger as Logger
import qualified UI.Butcher.Monadic as Butcher

data Command
  = StartServer Server.Config
  | StartAgent Agent.Config

runCommand :: Command -> IO ()
runCommand = \case
  StartServer config -> do
    Logger.infoM "ci.server" "Server starting..."
    handler <- JobHandler.Memory.mkService
    Server.run config handler
  StartAgent config -> do
    Logger.infoM "ci.agent" "Agent starting..."
    docker <- Docker.mkService
    let runner = Runner.mkService docker
    Agent.run config runner

defaultPort :: Int
defaultPort = 9000

defaultEndpoint :: String
defaultEndpoint = "http://localhost:" <> show defaultPort

main :: IO ()
main = Butcher.mainFromCmdParserWithHelpDesc $ \helpDesc -> do
  Butcher.addCmdSynopsis "CI command line utility"
  Butcher.addHelpCommand2 helpDesc

  Butcher.addCmd "start-server" do
    Butcher.addCmdSynopsis "Start server node"
    port <-
      Butcher.addParamString "PORT"
        $ Butcher.paramHelpStr "Server port"
        <> Butcher.paramDefault (show defaultPort)

    Butcher.addCmdImpl do
      case readMaybe port of
        Nothing -> throwString "Port must be a number"
        Just p -> do
          let config = Server.Config {Server.port = p}
          runCommand $ StartServer config

  Butcher.addCmd "start-agent" do
    Butcher.addCmdSynopsis "Start agent node"
    endpoint <-
      Butcher.addParamString "ENDPOINT"
        $ Butcher.paramHelpStr "Server endpoint"
        <> Butcher.paramDefault defaultEndpoint

    Butcher.addCmdImpl do
      let config = Agent.Config {Agent.endpoint = endpoint}
      runCommand $ StartAgent config
