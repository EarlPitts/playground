module JobHandler where

import qualified Agent
import Core
import Data.Aeson ((.:))
import qualified Data.Aeson as Aeson
import RIO
import qualified RIO.Text as Text

data Job = Job
  { pipeline :: Pipeline,
    state :: JobState,
    info :: CommitInfo
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
    branch :: Text,
    message :: Text,
    author :: Text,
    repo :: Text
  }
  deriving (Show, Eq)

instance Aeson.FromJSON CommitInfo where
  parseJSON = Aeson.withObject "CommitInfo" $ \obj -> do
    branch <- Text.dropPrefix "refs/heads/" <$> (obj .: "ref")
    repo <- obj .: "repository"
    name <- repo .: "full_name"
    head <- obj .: "head_commit"
    message <- head .: "message"
    author <- head .: "author" >>= \a -> a .: "username"
    sha <- head .: "id"
    pure $ CommitInfo sha branch message author name

data Service = Service
  { queueJob :: CommitInfo -> Pipeline -> IO BuildNumber,
    findJob :: BuildNumber -> IO (Maybe Job),
    dispatchCmd :: IO (Maybe Agent.Cmd),
    processMsg :: Agent.Msg -> IO (),
    fetchLogs :: BuildNumber -> StepName -> IO (Maybe ByteString),
    latestJobs :: IO [(BuildNumber, Job)]
  }
