module Main (main) where

import Core
import RIO
import RIO.NonEmpty.Partial as NE.P

mkStep :: Text -> [Text] -> Text -> Step
mkStep name commands image =
  Step
    { name = StepName name,
      commands = NE.P.fromList commands,
      image = Image image
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
main = pure ()
