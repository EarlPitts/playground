module Main (main) where

import qualified Agent
import qualified Control.Concurrent.Async as Async
import Core
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Yaml as Yaml
import qualified Docker
import qualified JobHandler
import qualified JobHandler.Memory
import qualified Network.HTTP.Simple as HTTP
import RIO
import qualified RIO.ByteString as BS
import qualified RIO.Map as Map
import RIO.NonEmpty.Partial as NE.P
import qualified RIO.Set as Set
import qualified Runner
import qualified Server
import qualified System.Process.Typed as Process
import Test.Hspec

mkStep :: Text -> [Text] -> Text -> Step
mkStep name commands image =
  Step
    { name = StepName name,
      commands = NE.P.fromList commands,
      image = Docker.Image image "latest"
    }

mkPipeline :: [Step] -> Pipeline
mkPipeline steps = Pipeline {steps = NE.P.fromList steps}

dockerStub :: Docker.Service
dockerStub =
  Docker.Service
    { Docker.createContainer = \_ -> pure (Docker.ContainerID mempty),
      Docker.startContainer = \_ -> pure (),
      Docker.containerStatus = \_ -> pure $ Docker.ContainerExited (Docker.ContainerExitCode 0),
      Docker.createVolume = pure $ Docker.Volume "",
      Docker.fetchLogs = \_ -> pure $ "log",
      Docker.pullImage = \_ -> pure ()
    }

runnerStub :: Runner.Service
runnerStub =
  Runner.Service
    { Runner.runBuild = \_ build ->
        pure $ build {state = BuildFinished BuildSucceeded},
      Runner.prepareBuild = \pipeline ->
        pure
          $ Build
            { pipeline = pipeline,
              state = BuildReady,
              completedSteps = mempty,
              volume = Docker.Volume ""
            }
    }

emptyHooks :: Runner.Hooks
emptyHooks =
  Runner.Hooks
    { Runner.logCollected = \_ -> pure (),
      Runner.buildUpdated = \_ -> pure ()
    }

cleanupDocker :: IO ()
cleanupDocker = void $ do
  Process.runProcess "docker rm -f $(docker ps -aq --filter \"label=ci\")"
  Process.runProcess "docker volume rm -f $(docker volume ls -q --filter \"label=ci\")"

checkBuild :: JobHandler.Service -> BuildNumber -> IO ()
checkBuild handler buildNumber = loop
  where
    loop = do
      Just job <- handler.findJob buildNumber
      case job.state of
        JobHandler.JobScheduled build -> case build.state of
          Core.BuildFinished res -> res `shouldBe` Core.BuildSucceeded
          _ -> loop
        _ -> loop

checkBuild' :: BuildNumber -> IO ()
checkBuild' buildNumber = loop
  where
    loop = do
      req <- HTTP.parseRequest $ "http://localhost:9000/build/" <> (show (buildNumberToInt buildNumber))
      res <- HTTP.httpBS req
      let (Just (Aeson.Object job)) = Aeson.decodeStrict $ HTTP.getResponseBody res
      traceShowIO job
      let (Just (Aeson.String status)) = KeyMap.lookup "state" job
      case status of
        "succeeded" -> pure ()
        _ -> loop

main :: IO ()
main = do
  hspec do
    docker <- runIO Docker.mkService
    let runner = Runner.mkService docker

    beforeAll cleanupDocker $ describe "CI" do
      -- it "should run a build (success)" do
      --   testRunSuccess runner
      -- it "should run a build (failure)" do
      --   testRunFailure runner
      -- it "should share workspace between steps" do
      --   testSharedWorkspace runner
      -- it "should collect logs" do
      --   testLogCollection runner
      -- it "should pull images" do
      --   testPullingImage runner
      -- it "parse and run pipeline" do
      --   testParsePipeline runner
      -- it "run build on agent" do
      --   testServerAndAgent runner
      it "should process webhooks" do
        testWebhookTrigger runner
      -- it "should reply with a build" do
      --   testBuildEndpoint runner

testRunSuccess :: Runner.Service -> IO ()
testRunSuccess runner = do
  build <-
    runner.prepareBuild
      $ mkPipeline
        [ mkStep "First step" ["date"] "ubuntu",
          mkStep "Second step" ["uname"] "ubuntu"
        ]
  result <- runner.runBuild emptyHooks build

  result.state `shouldBe` BuildFinished BuildSucceeded
  Map.elems result.completedSteps `shouldBe` [StepSucceeded, StepSucceeded]

testRunFailure :: Runner.Service -> IO ()
testRunFailure runner = do
  build <- runner.prepareBuild $ mkPipeline [mkStep "should fail" ["exit 1"] "ubuntu"]
  result <- runner.runBuild emptyHooks build

  result.state `shouldBe` BuildFinished BuildFailed
  Map.elems result.completedSteps `shouldBe` [StepFailed (Docker.ContainerExitCode 1)]

testSharedWorkspace :: Runner.Service -> IO ()
testSharedWorkspace runner = do
  build <-
    runner.prepareBuild
      $ mkPipeline
        [ mkStep "Create file" ["touch testfile"] "ubuntu",
          mkStep "Read file" ["[ -f testfile ]"] "ubuntu"
        ]
  result <- runner.runBuild emptyHooks build

  result.state `shouldBe` BuildFinished BuildSucceeded
  Map.elems result.completedSteps `shouldBe` [StepSucceeded, StepSucceeded]

testLogCollection :: Runner.Service -> IO ()
testLogCollection runner = do
  expected <- newMVar $ Set.fromList ["hello", "world", "Linux"]

  let onLog :: Log -> IO ()
      onLog log = do
        remaining <- readMVar expected
        forM_ remaining $ \word -> do
          if BS.isInfixOf word log.output
            then modifyMVar_ expected (pure . Set.delete word)
            else pure ()

  let hooks =
        Runner.Hooks
          { Runner.logCollected = onLog,
            Runner.buildUpdated = \_ -> pure ()
          }

  build <-
    runner.prepareBuild
      $ mkPipeline
        [ mkStep "Long step" ["echo hello", "sleep 2", "echo world"] "ubuntu",
          mkStep "Kernel name" ["uname"] "ubuntu"
        ]
  result <- runner.runBuild hooks build

  readMVar expected >>= (shouldBe Set.empty)

  result.state `shouldBe` BuildFinished BuildSucceeded
  Map.elems result.completedSteps `shouldBe` [StepSucceeded, StepSucceeded]

testPullingImage :: Runner.Service -> IO ()
testPullingImage runner = do
  Process.runProcess_ "docker rmi -f busybox"

  build <-
    runner.prepareBuild
      $ mkPipeline [mkStep "First step" ["date"] "busybox"]
  result <- runner.runBuild emptyHooks build

  result.state `shouldBe` BuildFinished BuildSucceeded
  Map.elems result.completedSteps `shouldBe` [StepSucceeded]

testParsePipeline :: Runner.Service -> IO ()
testParsePipeline runner = do
  pipeline <- Yaml.decodeFileThrow "test/pipeline.yaml"
  build <- runner.prepareBuild pipeline
  result <- runner.runBuild emptyHooks build
  result.state `shouldBe` BuildFinished BuildSucceeded
  Map.elems result.completedSteps `shouldBe` [StepSucceeded, StepSucceeded]

runServerAndAgent :: (JobHandler.Service -> IO ()) -> Runner.Service -> IO ()
runServerAndAgent callback runner = do
  handler <- JobHandler.Memory.mkService

  serverThread <- Async.async $ Server.run (Server.Config 9000) handler
  Async.link serverThread

  agentThread <- Async.async $ Agent.run (Agent.Config "http://localhost:9000") runner
  Async.link agentThread

  callback handler

  Async.cancel serverThread
  Async.cancel agentThread

testServerAndAgent :: Runner.Service -> IO ()
testServerAndAgent = runServerAndAgent $ \handler -> do
  let pipeline = mkPipeline [mkStep "agent-test" ["echo hello", "echo from agent"] "busybox"]

  buildNumber <- handler.queueJob pipeline
  checkBuild handler buildNumber

testWebhookTrigger :: Runner.Service -> IO ()
testWebhookTrigger = runServerAndAgent $ \handler -> do
  req <- HTTP.parseRequest "http://localhost:9000"

  res <-
    HTTP.httpBS
      $ req
      & HTTP.setRequestMethod "POST"
      & HTTP.setRequestPath "webhook/github"
      & HTTP.setRequestBodyFile "test/github-payload.sample.json"

  let Right (Aeson.Object build) = Aeson.eitherDecodeStrict $ HTTP.getResponseBody res
  let Just (Aeson.Number number) = KeyMap.lookup "number" build

  checkBuild handler $ BuildNumber (round number)

testBuildEndpoint :: Runner.Service -> IO ()
testBuildEndpoint = runServerAndAgent $ \handler -> do
  req <- HTTP.parseRequest "http://localhost:9000"

  res <-
    HTTP.httpBS
      $ req
      & HTTP.setRequestMethod "POST"
      & HTTP.setRequestPath "webhook/github"
      & HTTP.setRequestBodyFile "test/github-payload.sample.json"

  let Right (Aeson.Object build) = Aeson.eitherDecodeStrict $ HTTP.getResponseBody res
  let Just (Aeson.Number number) = KeyMap.lookup "number" build

  checkBuild' $ BuildNumber (round number)
