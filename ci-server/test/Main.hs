module Main (main) where

import Core
import qualified Docker
import RIO
import qualified RIO.Map as Map
import RIO.NonEmpty.Partial as NE.P
import qualified Socket
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

testSteps :: [Step]
testSteps =
  [ mkStep "First step" ["date"] "ubuntu",
    mkStep "Second step" ["uname"] "ubuntu"
  ]

testPipeline :: Pipeline
testPipeline = mkPipeline testSteps

testBuild :: Build
testBuild =
  Build
    { pipeline = testPipeline,
      state = BuildReady,
      completedSteps = mempty
    }

runBuild :: Docker.Service -> Build -> IO Build
runBuild docker build = do
  newBuild <- Core.progress docker build
  case newBuild.state of
    BuildFinished _ -> pure newBuild
    _ -> do
      threadDelay (1 * 1000 * 1000) -- We don't want to DoS the docker daemon
      runBuild docker newBuild

dockerStub :: Docker.Service
dockerStub =
  Docker.Service
    { Docker.createContainer = \_ -> pure (Docker.ContainerID mempty),
      Docker.startContainer = \_ -> pure ()
    }

main :: IO ()
main = do
  -- manager <- Socket.newManager "/run/docker.sock"
  -- let docker = Docker.mkService manager

  hspec do
    describe "CI" do
      it "should run a build (success)" do
        testRunSuccess dockerStub

testRunSuccess :: Docker.Service -> IO ()
testRunSuccess docker = do
  result <- runBuild docker testBuild
  result.state `shouldBe` BuildFinished BuildSucceeded
  Map.elems result.completedSteps `shouldBe` [StepSucceeded, StepSucceeded]
