module Main (main) where

import Control.Monad
import Core
import qualified Docker
import RIO
import RIO.NonEmpty.Partial as NE.P
import Test.Hspec
import Prelude (putStrLn)

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

main :: IO ()
main = hspec $ do
  describe "progress" $ do
    it "Finishes build when all steps ran" $ do
      let stepNum = length testSteps
      bs <- (f (8) testBuild)
      traverse_ (putStrLn . show) bs
      after <- foldM (\build _ -> progress build) testBuild [0 .. (stepNum * 2)]
      after.state `shouldBe` (BuildFinished BuildSucceeded)

f :: Int -> Build -> IO [Build]
f 0 _ = pure []
f n b = do
  curr <- progress b
  next <- f (n - 1) curr
  pure (b : next)
