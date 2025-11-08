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
    dispatchCmd :: IO Agent.Cmd,
    processMsg :: Agent.Msg -> IO (),
    findJob :: BuildNumber -> IO (Maybe Job)
  }

mkService :: Service
mkService =
  Service
    { queueJob = queueJob_,
      dispatchCmd = dispatchCmd_,
      processMsg = processMsg_,
      findJob = undefined
    }

queueJob_ :: Pipeline -> IO BuildNumber
queueJob_ pipeline = undefined

dispatchCmd_ :: IO Agent.Cmd
dispatchCmd_ = undefined

processMsg_ :: Agent.Msg -> IO ()
processMsg_ = undefined
