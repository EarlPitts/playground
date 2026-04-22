{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}

module HCat where

import Control.Concurrent
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
  -- input
  arg <- handleArgs
  path <- eitherToErr arg
  content <- T.readFile path
  (h, w) <- getDimensions >>= eitherToErr

  let pages = toPages h w content
  print $ length pages

  -- output
  showPages pages
 where
  handleIOError p =
    Exception.catch p $
      \e -> putStrLn $ "Error: " <> show @IOError e

groupsOf :: Int -> [a] -> [[a]]
groupsOf n = \case
  [] -> []
  as -> case splitAt n as of
    (h, t) -> h : groupsOf n t

newtype Page = Page {getPage :: [T.Text]}

showPages :: [Page] -> IO ()
showPages = \case
  [] -> pure ()
  (p : ps) -> do
    display p
    c <- getInput
    when (c == ' ') $ showPages ps

display :: Page -> IO ()
display (Page t) = T.putStr $ T.unlines t

toPages :: Int -> Int -> T.Text -> [Page]
toPages h w =
  fmap Page . groupsOf (h - 1) . concatMap (wordWrap w) . T.lines

wordWrap :: Int -> T.Text -> [T.Text]
wordWrap n t
  | T.length t <= n = [t]
  | otherwise =
      let (wrapped, unwrapped) = T.splitAt n t
       in wrapped : wordWrap n unwrapped

getInput :: IO Char
getInput = do
  hSetBuffering stdin NoBuffering
  hSetEcho stdin False
  c <- getChar
  hSetEcho stdin True
  if c `elem` [' ', 'q']
    then pure c
    else getInput

getDimensions :: IO (Either Error (Int, Int))
getDimensions = do
  ds <- size
  case ds of
    Just (Window h w) -> pure $ Right (h, w)
    Nothing -> pure $ Left CannotGetDimensions

data Error
  = NoFileSpecified
  | TooManyFiles
  | CannotGetDimensions
  deriving (Show)

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

displayError :: Error -> IO ()
displayError = \case
  TooManyFiles -> putStrLn "Error: Too many files given"
  NoFileSpecified -> putStrLn "Error: No file specified"
