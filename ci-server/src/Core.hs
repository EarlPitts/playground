{-# LANGUAGE FlexibleContexts #-}

module Core where

import qualified Docker
import RIO
import RIO.List as L
import qualified RIO.Map as M
import qualified RIO.NonEmpty as NE
import qualified RIO.Text as Text

data Pipeline = Pipeline
  { steps :: NonEmpty Step
  }
  deriving (Show, Eq)

data Step = Step
  { name :: StepName,
    commands :: NonEmpty Text,
    image :: Docker.Image
  }
  deriving (Show, Eq)

data Build = Build
  { pipeline :: Pipeline,
    state :: BuildState,
    completedSteps :: Map StepName StepResult,
    volume :: Docker.Volume
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
  | BuildUnexpectedState Text
  deriving (Show, Eq)

data BuildRunningState = BuildRunningState
  { step :: StepName,
    container :: Docker.ContainerID
  }
  deriving (Show, Eq)

data StepResult
  = StepSucceeded
  | StepFailed Docker.ContainerExitCode
  deriving (Show, Eq)

newtype StepName = StepName Text deriving (Show, Eq, Ord)

stepNameToText :: StepName -> Text
stepNameToText (StepName t) = t

exitCodeToInt :: Docker.ContainerExitCode -> Int
exitCodeToInt (Docker.ContainerExitCode code) = code

exitCodeToStepResult :: Docker.ContainerExitCode -> StepResult
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

progress :: Docker.Service -> Build -> IO Build
progress docker build =
  case build.state of
    BuildReady -> case buildHasNextStep build of
      Left result -> pure build {state = BuildFinished result}
      Right s -> do
        let script = Text.unlines $ ["set -ex"] <> NE.toList s.commands
        cid <- docker.createContainer (Docker.ContainerCreateOptions (s.image) script)
        docker.startContainer cid
        let runState = BuildRunningState {step = s.name, container = cid}
        pure $ build {state = BuildRunning runState}
    BuildRunning state -> do
      status <- docker.containerStatus state.container

      case status of
        Docker.ContainerRunning -> pure build
        Docker.ContainerExited code -> do
          let result = exitCodeToStepResult code
              updatedSteps = M.insert state.step result build.completedSteps
          pure build {state = BuildReady, completedSteps = updatedSteps}
        Docker.ContainerOther other ->
          pure build {state = BuildFinished (BuildUnexpectedState other)}
    BuildFinished _ -> pure build
