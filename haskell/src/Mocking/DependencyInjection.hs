module Mocking.DependencyInjection where

{-# LANGUAGE RankNTypes #-}

import Control.Monad.IO.Class (MonadIO, liftIO)
import System.IO
import Control.Monad.Reader

type Logger m = forall a. (Show a) => a -> m ()

noLogger :: (Monad m) => Logger m
noLogger = const $ return ()

stderrLogger :: (MonadIO m) => Logger m
stderrLogger = liftIO . hPrint stderr

printFile :: (MonadIO m) => Logger m -> FilePath -> m ()
printFile log fp = do
  log ("Printing file: " ++ fp)
  liftIO (readFile fp >>= putStr)
  log "Done printing"
