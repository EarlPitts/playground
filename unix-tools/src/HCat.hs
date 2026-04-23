{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module HCat where

import qualified Control.Exception as Exception
import Control.Monad (when)
import qualified Control.Monad.Except as Except
import qualified Data.Text as T
import qualified Data.Text.IO as T
import System.Console.Terminal.Size
import System.Environment
import System.IO (BufferMode (NoBuffering), hSetBuffering, hSetEcho, stdin)
import System.IO.Error

runHCat :: IO ()
runHCat = handleIOError $ do
  arg <- handleArgs
  path <- eitherToErr arg
  content <- T.readFile path
  dimensions <- getDimensions >>= eitherToErr

  let pages = paginate dimensions content

  showPages pages
 where
  handleIOError p =
    Exception.catch p $
      \e -> putStrLn $ "Error: " <> show @IOError e

showPages :: [T.Text] -> IO ()
showPages = \case
  [] -> pure ()
  (p : ps) -> do
    T.putStr p
    c <- getInput
    when (c == ' ') $ showPages ps

data ScreenDimensions = ScreenDimensions
  { rows :: Int
  , cols :: Int
  }
  deriving (Show)

data Error
  = NoFileSpecified
  | TooManyFiles
  | CannotGetDimensions
  deriving (Show)

-- IO
getInput :: IO Char
getInput = do
  hSetBuffering stdin NoBuffering
  hSetEcho stdin False
  c <- getChar
  hSetEcho stdin True
  if c `elem` [' ', 'q']
    then pure c
    else getInput

getDimensions :: IO (Either Error ScreenDimensions)
getDimensions =
  size >>= \case
    Just (Window r c) -> pure $ Right (ScreenDimensions r c)
    Nothing -> pure $ Left CannotGetDimensions

eitherToErr :: (Show a) => Either a b -> IO b
eitherToErr = \case
  Left err -> Exception.throwIO $ userError (show err)
  Right b -> pure b

handleArgs :: IO (Either Error FilePath)
handleArgs = parseArgs <$> getArgs
 where
  parseArgs = \case
    [path] -> Right path
    [] -> Left NoFileSpecified
    _ -> Left TooManyFiles

-- Core
paginate :: ScreenDimensions -> T.Text -> [T.Text]
paginate ScreenDimensions{..} t = T.unlines <$> pageLines
 where
  unwrappedLines = T.lines t
  wrappedLines = concatMap (wordWrap cols) unwrappedLines
  pageLines = groupsOf (rows - 1) wrappedLines

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

groupsOf :: Int -> [a] -> [[a]]
groupsOf n = \case
  [] -> []
  as -> case splitAt n as of
    (h, t) -> h : groupsOf n t
