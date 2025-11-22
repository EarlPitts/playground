module JobHandler where

import qualified Agent
import Core
import Data.Aeson ((.:))
import qualified Data.Aeson as Aeson
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

data CommitInfo
  = CommitInfo
  { sha :: Text,
    repo :: Text
  }
  deriving (Show, Eq)

instance Aeson.FromJSON CommitInfo where
  parseJSON = Aeson.withObject "CommitInfo" $ \obj -> do
    repo <- obj .: "repository"
    name <- repo .: "full_name"
    head <- obj .: "head_commit"
    sha <- head .: "id"
    pure $ CommitInfo sha name

data Service = Service
  { queueJob :: Pipeline -> IO BuildNumber,
    findJob :: BuildNumber -> IO (Maybe Job),
    dispatchCmd :: IO (Maybe Agent.Cmd),
    processMsg :: Agent.Msg -> IO (),
    fetchLogs :: BuildNumber -> StepName -> IO (Maybe ByteString),
    latestJobs :: IO [(BuildNumber, Job)]
  }
