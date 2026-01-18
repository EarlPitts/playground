module GuessTheNumber where

import System.Console.Haskeline
import System.Random
import Text.Read
import Control.Monad (when)

debugMode :: Bool
debugMode = True

getInput :: IO (Maybe String)
getInput =
  runInputT
    defaultSettings
    (getInputLine "Enter your guess: ")

main :: IO ()
main = do
  num <- randomRIO (0, 100) :: IO Int
  when debugMode (print num)
  numOfGuesses <- gameLoop num
  case numOfGuesses of
    Nothing -> pure ()
    Just n -> putStrLn $ "It took " <> show n <> " guesse(s) to win"
  putStrLn "Bye!"

gameLoop :: Int -> IO (Maybe Int)
gameLoop num = go 1
  where
    go numOfGuesses = do
      input <- getInput
      case readMaybe <$> input of
        Nothing -> pure Nothing
        Just Nothing -> do
          putStrLn "Please enter a valid number!"
          go numOfGuesses
        Just (Just guess) -> do
          if guess == num
            then do
              putStrLn "Yep, that was it!"
              pure (Just numOfGuesses)
            else do
              putStrLn $
                if guess > num
                  then "Nope, lower"
                  else "Nope, higher"
              go (succ numOfGuesses)
