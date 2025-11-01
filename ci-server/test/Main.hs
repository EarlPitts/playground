module Main (main) where

import Core
import qualified Docker
import RIO
import qualified RIO.Map as Map
import RIO.NonEmpty.Partial as NE.P
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
      Docker.createVolume = pure $ Docker.Volume ""
    }

runnerStub :: Runner.Service
runnerStub =
  Runner.Service
    { Runner.runBuild = \build ->
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

cleanupDocker :: IO ()
cleanupDocker = void $ Process.runProcess "docker rm -f $(docker ps -aq --filter \"label=ci\")"

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
      it "should find the created file" do
        f runner

testRunSuccess :: Runner.Service -> IO ()
testRunSuccess runner = do
  build <-
    runner.prepareBuild
      $ mkPipeline
        [ mkStep "First step" ["date"] "ubuntu",
          mkStep "Second step" ["uname"] "ubuntu"
        ]
  result <- runner.runBuild build

  result.state `shouldBe` BuildFinished BuildSucceeded
  Map.elems result.completedSteps `shouldBe` [StepSucceeded, StepSucceeded]

testRunFailure :: Runner.Service -> IO ()
testRunFailure runner = do
  build <- runner.prepareBuild $ mkPipeline [mkStep "should fail" ["exit 1"] "ubuntu"]
  result <- runner.runBuild build

  result.state `shouldBe` BuildFinished BuildFailed
  Map.elems result.completedSteps `shouldBe` [StepFailed (Docker.ContainerExitCode 1)]

f :: Runner.Service -> IO ()
f runner = do
  build <-
    runner.prepareBuild
      $ mkPipeline
        [ mkStep "First step" ["touch testfile"] "ubuntu",
          mkStep "Second step" ["[ -f testfile ]"] "ubuntu"
        ]
  result <- runner.runBuild build

  result.state `shouldBe` BuildFinished BuildSucceeded
  Map.elems result.completedSteps `shouldBe` [StepSucceeded, StepSucceeded]
