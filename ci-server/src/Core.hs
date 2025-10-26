{-# LANGUAGE FlexibleContexts #-}

module Core where

import RIO
import RIO.List as L
import qualified RIO.Map as M
import qualified RIO.NonEmpty as NE
import qualified RIO.NonEmpty.Partial as NE.P

data Pipeline = Pipeline
  { steps :: NonEmpty Step
  }
  deriving (Show, Eq)

data Step = Step
  { name :: StepName,
    commands :: NonEmpty Text,
    image :: Image
  }
  deriving (Show, Eq)

data Build = Build
  { pipeline :: Pipeline,
    state :: BuildState,
    completedSteps :: Map StepName StepResult
  }
  deriving (Show, Eq)

data BuildState
  = BuildReady
  | BuildRunning BuildRunningState
  | BuildFinished BuildResult
  deriving (Show, Eq)

data BuildResult
  = BuildFailed
  | BuildSucceeded
  deriving (Show, Eq)

data BuildRunningState = BuildRunningState
  { step :: StepName
  }
  deriving (Show, Eq)

data StepResult
  = StepSucceeded
  | StepFailed ContainerExitCode
  deriving (Show, Eq)

newtype ContainerExitCode = ContainerExitCode Int deriving (Show, Eq)

newtype Image = Image Text deriving (Show, Eq)

newtype StepName = StepName Text deriving (Show, Eq, Ord)

imageToText :: Image -> Text
imageToText (Image t) = t

stepNameToText :: StepName -> Text
stepNameToText (StepName t) = t

exitCodeToInt :: ContainerExitCode -> Int
exitCodeToInt (ContainerExitCode code) = code

exitCodeToStepResult :: ContainerExitCode -> StepResult
exitCodeToStepResult code =
  if exitCodeToInt code == 0
    then StepSucceeded
    else StepFailed code

buildHasNextStep :: Build -> Either BuildResult Step
buildHasNextStep build =
  if allSucceeded
    then case nextStep of
      Nothing -> Left BuildSucceeded
      Just step -> Right step
    else Left BuildFailed
  where
    allSucceeded = L.all ((==) StepSucceeded) build.completedSteps
    nextStep = L.find f build.pipeline.steps
    f step = not $ M.member step.name build.completedSteps

progress :: Build -> IO Build
progress build =
  case build.state of
    BuildReady -> case buildHasNextStep build of
      Left result -> pure build {state = BuildFinished result}
      Right s -> pure build {state = BuildRunning (BuildRunningState s.name)}
    BuildRunning state -> do
      let code = ContainerExitCode 0
          result = exitCodeToStepResult code
          updatedSteps = M.insert state.step result build.completedSteps
      pure build {state = BuildReady, completedSteps = updatedSteps}
    BuildFinished _ -> pure build
