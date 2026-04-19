{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}

module HCat where

import qualified Control.Exception as Exception
import qualified Control.Monad.Except as Except
import qualified Data.Text as T
import qualified Data.Text.IO as T
import System.Console.Terminal.Size
import System.Environment
import System.IO.Error

runHCat :: IO ()
runHCat = handleIOError $ do
  -- input
  arg <- handleArgs
  path <- eitherToErr arg
  content <- T.readFile path
  height <- getHeight <$> size

  -- output
  T.putStr content
 where
  handleIOError p =
    Exception.catch p $
      \e -> putStrLn $ "Error: " <> show @IOError e

groupsOf :: Int -> [a] -> [[a]]
groupsOf n = \case
  [] -> []
  as -> case splitAt n as of
    (h, t) -> h : groupsOf n t

getHeight :: Maybe (Window Int) -> Int
getHeight = \case
  Just (Window h _) -> h
  Nothing -> error "baj van"

data Error
  = NoFileSpecified
  | TooManyFiles
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
