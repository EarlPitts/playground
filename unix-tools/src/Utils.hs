{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}

module Utils where

import Control.Exception (IOException, handle)
import Control.Monad
import qualified Data.ByteString as BS
import Data.Foldable
import Data.IORef
import Data.List
import qualified Data.Set as S
import System.Directory as D

data FileType
  = FileTypeRegularFile
  | FileTypeDirectory
  | FileTypeOther
  deriving (Show, Eq)

classifyFile :: FilePath -> IO FileType
classifyFile path = do
  isDirectory <- D.doesDirectoryExist path
  isFile <- D.doesFileExist path
  case (isDirectory, isFile) of
    (True, False) -> pure FileTypeDirectory
    (False, True) -> pure FileTypeRegularFile
    _ -> pure FileTypeOther

dropSuffix :: (Eq a) => [a] -> [a] -> [a]
dropSuffix suffix as
  | suffix `isSuffixOf` as =
      take (length as - length suffix) as
  | otherwise = as

traverseDirectory :: FilePath -> (FilePath -> IO ()) -> IO ()
traverseDirectory rootPath action = do
  seenRef <- newIORef S.empty

  let haveSeenDriectory canonicalPath =
        S.member canonicalPath <$> readIORef seenRef

      addDirectoryToSeen canonicalPath =
        modifyIORef seenRef $ S.insert canonicalPath

      traverseSubDirectory subdirPath = do
        contents <- listDirectory subdirPath
        for_ contents $ \file' ->
          handle @IOException (\_ -> pure ()) $ do
            let file = subdirPath <> "/" <> file'
            canonicalPath <- canonicalizePath file
            classifyFile canonicalPath >>= \case
              FileTypeOther -> pure ()
              FileTypeRegularFile -> action file
              FileTypeDirectory -> do
                alreadyProcessed <- haveSeenDriectory file
                unless alreadyProcessed $ do
                  addDirectoryToSeen file
                  traverseSubDirectory file

  traverseSubDirectory (dropSuffix "/" rootPath)

traverseDirectory' :: FilePath -> (FilePath -> a) -> IO [a]
traverseDirectory' path action = do
  resultRef <- newIORef []
  traverseDirectory path $ \file ->
    modifyIORef resultRef (action file :)
  readIORef resultRef

largestContents :: FilePath -> IO BS.ByteString
largestContents rootPath = do
  contentsRef <- newIORef BS.empty

  traverseDirectory rootPath $ \file -> do
    contents <- BS.readFile file
    modifyIORef contentsRef (takeLongest contents)

  readIORef contentsRef
 where
  takeLongest a b = do
    if BS.length a > BS.length b
      then a
      else b
