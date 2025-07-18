module MTL.MTL where

import Control.Monad.Catch
import Control.Monad.Reader
import Data.Function
import Data.Time
import Control.Monad.Identity

data Logger m = Logger
  { debug :: String -> m (),
    info :: String -> m (),
    warning :: String -> m ()
  }

data Http m = Http
  { getUsers :: m [String],
    postUser :: String -> m ()
  }

data Database m = Database
  { dbHistory :: m [String],
    dbAdd :: String -> m ()
  }

data OpenSpan = OpenSpan
  { spanStart :: UTCTime,
    spanName :: String,
    spanId :: Int,
    spanParent :: Maybe Int
  } deriving Show

data Tracer m = Tracer
  { openSpan ::
      Maybe Int ->
      String ->
      m OpenSpan,
    closeSpan :: OpenSpan -> m ()
  }

type MonadTraced m = (MonadReader OpenSpan m, MonadMask m)

span :: (MonadTraced m) => Tracer m -> String -> m a -> m a
span tracer name ma = do
  OpenSpan {spanId} <- ask
  child <- (tracer & openSpan) (Just spanId) name
  local (const child) $ ma `finally` (tracer & closeSpan) child


time = UTCTime (fromGregorian 1998 1 1) (timeOfDayToTime (TimeOfDay 12 12 12))

mockTracer :: Tracer Identity
mockTracer = Tracer (\_ _ -> pure (OpenSpan time "" 0 Nothing)) (\_ -> pure ())
