module JobHandler where

import qualified Agent
import Core
import RIO

data Job = Job
  { pipeline :: Pipeline,
    state :: JobState
  }
  deriving (Show, Eq)

data JobState
  = JobQueued
  | JobAssigned
  | JobScheduled Build
  deriving (Show, Eq)

data Service = Service
  { queueJob :: Pipeline -> IO BuildNumber,
    findJob :: BuildNumber -> IO (Maybe Job),
    dispatchCmd :: IO (Maybe Agent.Cmd),
    processMsg :: Agent.Msg -> IO ()
  }
