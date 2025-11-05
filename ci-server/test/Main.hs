module Main (main) where

import Core
import qualified Docker
import RIO
import qualified RIO.ByteString as BS
import qualified RIO.Map as Map
import RIO.NonEmpty.Partial as NE.P
import qualified RIO.Set as Set
import qualified Runner
import qualified System.Process.Typed as Process
import Test.Hspec

mkStep :: Text -> [Text] -> Text -> Step
mkStep name commands image =
  Step
    { name = StepName name,
      commands = NE.P.fromList commands,
      image = Docker.Image image
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
      Docker.fetchLogs = \_ -> pure $ "log"
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
    { Runner.logCollected = \_ -> pure ()
    }

cleanupDocker :: IO ()
cleanupDocker = void $ do
  Process.runProcess "docker rm -f $(docker ps -aq --filter \"label=ci\")"
  Process.runProcess "docker volume rm -f $(docker volume ls -q --filter \"label=ci\")"

main :: IO ()
main = do
  hspec do
    docker <- runIO Docker.mkService
    let runner = Runner.mkService docker

    beforeAll cleanupDocker $ describe "CI" do
      it "should run a build (success)" do
        testRunSuccess runner
      it "should run a build (failure)" do
        testRunFailure runner
      it "should share workspace between steps" do
        testSharedWorkspace runner
      it "should collect logs" do
        testLogCollection runner

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

  let hooks = Runner.Hooks {Runner.logCollected = onLog}

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
