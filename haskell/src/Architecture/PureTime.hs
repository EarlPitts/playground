module Architecture.PureTime where

import Data.Time (NominalDiffTime, addUTCTime)
import Data.Time.Clock (UTCTime)
import Control.Monad.Free

newtype IdleDuration = IdleDuration NominalDiffTime deriving (Show, Eq)

newtype PollDuration = PollDuration NominalDiffTime deriving (Show, Eq)

newtype HandleDuration = HandleDuration NominalDiffTime deriving (Show, Eq)

data CycleDuration = CucleDuration
  { pollDuration :: PollDuration,
    handleDuration :: HandleDuration
  }
  deriving (Show, Eq)

data PollingState msg
  = Ready [CycleDuration]
  | ReceivedMessage [CycleDuration] PollDuration msg
  | NoMessage [CycleDuration] PollDuration
  | Stopped [CycleDuration]
  deriving (Show, Eq)

data PollingInstruction msg next
  = CurrentTime (UTCTime -> next)
  | Poll ((Maybe msg, PollDuration) -> next)
  | Handle msg (HandleDuration -> next)
  | Idle IdleDuration (IdleDuration -> next)
  deriving (Functor)
  
type PollingProgram msg = Free (PollingInstruction msg)

currentTime :: PollingProgram msg UTCTime
currentTime = liftF (CurrentTime id)

poll :: PollingProgram msg (Maybe msg, PollDuration)
poll = liftF (Poll id)

handle :: msg -> PollingProgram msg HandleDuration
handle msg = liftF (Handle msg id)

idle :: IdleDuration -> PollingProgram msg IdleDuration
idle d = liftF (Idle d id)

shouldIdle :: IdleDuration -> UTCTime -> PollingProgram msg Bool
shouldIdle (IdleDuration d) stopBefore = do
  now <- currentTime
  return $ d `addUTCTime` now < stopBefore
