{-# LANGUAGE FlexibleContexts #-}

module JobHandler.Memory where

import qualified Agent
import qualified Control.Concurrent.STM as STM
import Core
import qualified JobHandler
import RIO
import qualified RIO.List as List
import qualified RIO.Map as Map

data State = State
  { jobs :: Map BuildNumber JobHandler.Job,
    logs :: Map (BuildNumber, StepName) ByteString,
    nextBuild :: Int
  }
  deriving (Eq, Show)

mkService :: IO JobHandler.Service
mkService = do
  state <-
    STM.newTVarIO
      $ State
        { jobs = mempty,
          logs = mempty,
          nextBuild = 1
        }

  pure
    $ JobHandler.Service
      { JobHandler.queueJob = \info pipeline ->
          STM.atomically
            $ STM.stateTVar state
            $ queueJob_ info pipeline,
        JobHandler.findJob = \build -> do
          s <- STM.readTVarIO state
          pure $ findJob_ build s,
        JobHandler.dispatchCmd =
          STM.atomically
            $ STM.stateTVar state dispatchCmd_,
        JobHandler.processMsg = \msg ->
          STM.atomically
            $ STM.modifyTVar' state
            $ processMsg_ msg,
        JobHandler.fetchLogs = \number step -> STM.atomically do
          s <- STM.readTVar state
          pure $ fetchLogs_ number step s,
        JobHandler.latestJobs = STM.atomically do
          s <- STM.readTVar state
          pure $ latestJobs_ s
      }

queueJob_ :: JobHandler.CommitInfo -> Pipeline -> State -> (BuildNumber, State)
queueJob_ info pipeline s = (buildNum, newState)
  where
    job =
      JobHandler.Job
        { JobHandler.pipeline = pipeline,
          JobHandler.state = JobHandler.JobQueued,
          JobHandler.info = info
        }
    newState =
      s
        { jobs = Map.insert buildNum job s.jobs,
          nextBuild = s.nextBuild + 1
        }
    buildNum = BuildNumber s.nextBuild

findJob_ :: BuildNumber -> State -> Maybe JobHandler.Job
findJob_ build s = Map.lookup build s.jobs

dispatchCmd_ :: State -> (Maybe Agent.Cmd, State)
dispatchCmd_ s =
  case List.find queued $ Map.toList s.jobs of
    Just (build, job) ->
      let updatedJob = job {JobHandler.state = JobHandler.JobAssigned}
          updatedState = Map.insert build updatedJob s.jobs
          cmd = Just $ Agent.StartBuild build job.pipeline
       in (cmd, s {jobs = updatedState})
    Nothing -> (Nothing, s)
  where
    queued (_, job) = job.state == JobHandler.JobQueued

processMsg_ :: Agent.Msg -> State -> State
processMsg_ msg s = case msg of
  Agent.LogCollected buildNum log ->
    s {logs = Map.insertWith (flip mappend) (buildNum, log.step) log.output s.logs}
  Agent.BuildUpdated buildNum build ->
    let f job = job {JobHandler.state = JobHandler.JobScheduled build}
     in s {jobs = Map.adjust f buildNum s.jobs}

fetchLogs_ :: BuildNumber -> StepName -> State -> Maybe ByteString
fetchLogs_ number step state = Map.lookup (number, step) state.logs

latestJobs_ :: State -> [(BuildNumber, JobHandler.Job)]
latestJobs_ state = List.reverse $ Map.toList state.jobs
