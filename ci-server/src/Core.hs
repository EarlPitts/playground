{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}

module Core where

import qualified Codec.Serialise as Serialise
import qualified Data.Aeson as Aeson
import qualified Data.Time.Clock.POSIX as Time
import qualified Docker
import RIO
import RIO.List as L
import qualified RIO.Map as M
import qualified RIO.NonEmpty as NE
import qualified RIO.Text as Text

data Pipeline = Pipeline
  { steps :: NonEmpty Step
  }
  deriving (Show, Eq, Generic, Aeson.FromJSON, Serialise.Serialise)

data Step = Step
  { name :: StepName,
    commands :: NonEmpty Text,
    image :: Docker.Image
  }
  deriving (Show, Eq, Generic, Aeson.FromJSON, Serialise.Serialise)

data Build = Build
  { pipeline :: Pipeline,
    state :: BuildState,
    completedSteps :: Map StepName StepResult,
    volume :: Docker.Volume
  }
  deriving (Show, Eq, Generic, Serialise.Serialise)

newtype BuildNumber = BuildNumber Int deriving (Show, Eq, Ord, Generic, Serialise.Serialise)

data BuildState
  = BuildReady
  | BuildRunning BuildRunningState
  | BuildFinished BuildResult
  deriving (Show, Eq, Generic, Serialise.Serialise)

data BuildResult
  = BuildFailed
  | BuildSucceeded
  | BuildUnexpectedState Text
  deriving (Show, Eq, Generic, Serialise.Serialise)

data BuildRunningState = BuildRunningState
  { step :: StepName,
    container :: Docker.ContainerID
  }
  deriving (Show, Eq, Generic, Serialise.Serialise)

data StepResult
  = StepSucceeded
  | StepFailed Docker.ContainerExitCode
  deriving (Show, Eq, Generic, Serialise.Serialise)

newtype StepName = StepName Text deriving (Show, Eq, Ord, Generic, Aeson.FromJSON, Serialise.Serialise)

type LogCollection = Map StepName CollectionStatus

data CollectionStatus
  = CollectionReady
  | CollectingLogs Docker.ContainerID Time.POSIXTime
  | CollectionFinished
  deriving (Show, Eq)

data Log = Log
  { output :: ByteString,
    step :: StepName
  }
  deriving (Show, Eq, Generic, Serialise.Serialise)

stepNameToText :: StepName -> Text
stepNameToText (StepName t) = t

exitCodeToInt :: Docker.ContainerExitCode -> Int
exitCodeToInt (Docker.ContainerExitCode code) = code

buildNumberToInt :: BuildNumber -> Int
buildNumberToInt (BuildNumber n) = n

displayBuildNumber :: BuildNumber -> String
displayBuildNumber number = "#" <> show (buildNumberToInt number)

exitCodeToStepResult :: Docker.ContainerExitCode -> StepResult
exitCodeToStepResult code =
  if exitCodeToInt code == 0
    then StepSucceeded
    else StepFailed code

runCollection ::
  Docker.Service ->
  Time.POSIXTime ->
  LogCollection ->
  IO [Log]
runCollection docker until collection = do
  logs <- M.traverseWithKey f collection
  pure $ concat $ M.elems logs
  where
    f step = \case
      CollectingLogs container from -> do
        let options =
              Docker.FetchLogsOptions
                { container = container,
                  from = from,
                  to = until
                }
        output <- docker.fetchLogs options
        pure $ [Log {step = step, output = output}]
      CollectionReady -> pure []
      CollectionFinished -> pure []

collectLogs ::
  Docker.Service ->
  LogCollection ->
  Build ->
  IO (LogCollection, [Log])
collectLogs docker collection build = do
  now <- Time.getPOSIXTime
  logs <- runCollection docker now collection
  let newCollection = updateLogCollection build.state now collection
  pure (newCollection, logs)

initLogCollection :: Pipeline -> LogCollection
initLogCollection pipeline =
  M.fromList
    $ NE.toList pipeline.steps
    <&> \step -> (step.name, CollectionReady)

updateLogCollection :: BuildState -> Time.POSIXTime -> LogCollection -> LogCollection
updateLogCollection state lastCollection collection =
  M.mapWithKey f collection
  where
    update step since nextState = case state of
      BuildRunning state ->
        if state.step == step
          then CollectingLogs state.container since
          else nextState
      _ -> nextState
    f step = \case
      CollectionReady ->
        update step 0 CollectionReady
      CollectingLogs _ _ ->
        update step lastCollection CollectionFinished
      CollectionFinished ->
        CollectionFinished

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
        docker.pullImage s.image
        cid <-
          docker.createContainer
            $ Docker.ContainerCreateOptions (s.image) script build.volume
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
