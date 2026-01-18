module GuessTheNumber where

import Control.Monad (when)
import System.Console.Haskeline
import System.Random
import Text.Read

data Decision = EndGame | Retake | Lower | Higher | Guessed deriving (Show, Eq)

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

process :: Maybe String -> Int -> Decision
process input num = case readMaybe <$> input of
  Nothing -> EndGame
  Just Nothing -> Retake
  Just (Just guess) ->
    if guess == num
      then Guessed
      else
        if guess > num
          then Lower
          else Higher

gameLoop :: Int -> IO (Maybe Int)
gameLoop num = go 1
  where
    go numOfGuesses = do
      input <- getInput -- impure
      let decision = process input num -- pure
      case decision of -- impure
        EndGame -> pure Nothing
        Retake -> do
          putStrLn "Please enter a valid number!"
          go numOfGuesses
        Guessed -> do
          putStrLn "Yep, that was it!"
          pure (Just numOfGuesses)
        Lower -> do
          putStrLn "Nope, lower"
          go (succ numOfGuesses)
        Higher -> do
          putStrLn "Nope, higher"
          go (succ numOfGuesses)
