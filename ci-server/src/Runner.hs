{-# LANGUAGE FlexibleContexts #-}

module Runner where

import Core
import qualified Docker
import RIO

data Service = Service
  { runBuild :: Hooks -> Build -> IO Build,
    prepareBuild :: Pipeline -> IO Build
  }

data Hooks = Hooks
  { logCollected :: Log -> IO ()
  }

mkService :: Docker.Service -> Service
mkService docker =
  Service
    { runBuild = runBuild_ docker,
      prepareBuild = prepareBuild_ docker
    }

runBuild_ :: Docker.Service -> Hooks -> Build -> IO Build
runBuild_ docker hooks build = loop build $ Core.initLogCollection build.pipeline
  where
    loop build logCollection = do
      (newLogCollection, logs) <- Core.collectLogs docker logCollection build
      traverse_ hooks.logCollected logs
      newBuild <- Core.progress docker build
      case newBuild.state of
        BuildFinished _ -> pure newBuild
        _ -> do
          threadDelay (1 * 1000 * 1000) -- We don't want to DoS the docker daemon
          loop newBuild newLogCollection

prepareBuild_ :: Docker.Service -> Pipeline -> IO Build
prepareBuild_ docker pipeline = do
  volume <- docker.createVolume
  pure
    Build
      { pipeline = pipeline,
        state = BuildReady,
        completedSteps = mempty,
        volume = volume
      }
