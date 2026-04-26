{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module HCat where

import Control.Applicative (ZipList (..))
import qualified Control.Exception as Exception
import Control.Monad (when)
import qualified Control.Monad.Except as Except
import qualified Data.ByteString as BS
import Data.Foldable
import Data.List.Zipper
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Time.Clock as C
import qualified Data.Time.Clock.POSIX as PC
import qualified Data.Time.Format as TF
import qualified System.Console.Terminal.Size as Term
import qualified System.Directory as D
import System.Environment
import System.IO (BufferMode (NoBuffering), hSetBuffering, hSetEcho, stdin, stdout)
import System.IO.Error
import Text.Printf (printf)

runHCat :: IO ()
runHCat = runHCatWith Term.size

runHCatWith ::
  IO (Maybe (Term.Window Int)) ->
  IO ()
runHCatWith size = handleIOError $ do
  paths <- eitherToErr =<< handleArgs
  contents <- traverse T.readFile paths
  infos <- traverse fileInfo paths
  dimensions <- getDimensions size
  hSetBuffering stdout NoBuffering

  let pages = concat $ zipWith (paginate dimensions) infos contents
  let pages' = fromList pages

  showPages pages'
 where
  handleIOError p =
    Exception.catch p $
      \e -> putStrLn $ "Error: " <> show @IOError e

showPages :: Zipper T.Text -> IO ()
showPages pages
  | endp pages = pure ()
  | otherwise = do
      clearScreen
      T.putStr (cursor pages)
      getUserInput >>= \case
        Forward -> showPages (right pages)
        Backward -> showPages (left pages)
        Cancel -> pure ()

clearScreen :: IO ()
clearScreen =
  BS.putStr "\^[[1J\^[[1;1H"

data ScreenDimensions = ScreenDimensions
  { rows :: Int
  , cols :: Int
  }
  deriving (Show)

defaultScreenDimensions :: ScreenDimensions
defaultScreenDimensions = ScreenDimensions 20 40

data Error
  = NoFileSpecified
  | CannotGetDimensions
  deriving (Show)

data UserInput
  = Forward
  | Backward
  | Cancel
  deriving (Eq, Show)

data FileInfo = FileInfo
  { filePath :: FilePath
  , fileSize :: Int
  , fileMTime :: C.UTCTime
  , fileReadable :: Bool
  , fileWriteable :: Bool
  , fileExecutable :: Bool
  }
  deriving (Show)

-- IO
getUserInput :: IO UserInput
getUserInput = do
  hSetBuffering stdin NoBuffering
  hSetEcho stdin False
  c <- getChar
  case c of
    ' ' -> pure Forward
    '\DEL' -> pure Backward
    'j' -> pure Forward
    'k' -> pure Backward
    'q' -> pure Cancel
    _ -> getUserInput

getDimensions ::
  IO (Maybe (Term.Window Int)) ->
  IO ScreenDimensions
getDimensions size =
  size >>= \case
    Just (Term.Window r c) -> pure $ ScreenDimensions r c
    Nothing -> pure defaultScreenDimensions

eitherToErr :: (Show a) => Either a b -> IO b
eitherToErr = \case
  Left err -> Exception.throwIO $ userError (show err)
  Right b -> pure b

handleArgs :: IO (Either Error [FilePath])
handleArgs = parseArgs <$> getArgs
 where
  parseArgs = \case
    [] -> Left NoFileSpecified
    paths -> Right paths

fileInfo :: FilePath -> IO FileInfo
fileInfo path = do
  size <- fromIntegral <$> D.getFileSize path
  mtime <- D.getModificationTime path
  perm <- D.getPermissions path
  pure
    FileInfo
      { filePath = path
      , fileSize = size
      , fileMTime = mtime
      , fileReadable = D.readable perm
      , fileWriteable = D.writable perm
      , fileExecutable = D.executable perm
      }

-- Core
paginate :: ScreenDimensions -> FileInfo -> T.Text -> [T.Text]
paginate ScreenDimensions{..} info t = zipWith (<>) pages statusLines
 where
  rows' = rows - 1
  wrappedLines = concatMap (wordWrap cols) (T.lines t)
  pages = T.unlines . padTo rows' <$> groupsOf rows' wrappedLines
  pageCount = length pages
  statusLines = formatFileInfo info cols pageCount <$> [1 ..]
  padTo lineCount rs =
    take lineCount $ rs <> repeat ""

wordWrap :: Int -> T.Text -> [T.Text]
wordWrap n t
  | T.length t <= n = [t]
  | otherwise =
      let (candidate, rest) = T.splitAt n t
          (line, overflow) = softWrap candidate (T.length candidate - 1)
       in line : wordWrap n (overflow <> rest)
 where
  softWrap text idx
    | idx <= 0 = (text, T.empty)
    | T.index text idx == ' ' = T.tail <$> T.splitAt idx text
    | otherwise = softWrap text (idx - 1)

formatFileInfo :: FileInfo -> Int -> Int -> Int -> T.Text
formatFileInfo FileInfo{..} maxWidth totalPages currentPage =
  invertText (truncateStatus statusline)
 where
  invertText inputStr =
    let
      reverseVideo = "\^[[7m"
      resetVideo = "\^[[0m"
     in
      reverseVideo <> inputStr <> resetVideo
  truncateStatus statusLine
    | maxWidth <= 3 = ""
    | T.length statusLine > maxWidth =
        T.take (maxWidth - 3) statusLine <> "..."
    | otherwise = statusLine
  permissions =
    [ if fileReadable then 'r' else '-'
    , if fileWriteable then 'w' else '-'
    , if fileExecutable then 'x' else '-'
    ]
  timestamp = TF.formatTime TF.defaultTimeLocale "%F %T" fileMTime
  statusline =
    T.pack $
      printf
        "%s | permissions: %s | %d bytes | modified: %s | page: %d of %d"
        filePath
        permissions
        fileSize
        timestamp
        currentPage
        totalPages

groupsOf :: Int -> [a] -> [[a]]
groupsOf n = \case
  [] -> []
  as -> case splitAt n as of
    (h, t) -> h : groupsOf n t
