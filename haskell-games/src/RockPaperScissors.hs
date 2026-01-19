module RockPaperScissors where

import GHC.Generics (Generic)
import System.Console.Haskeline
import System.Random.Stateful
import Text.Read (readMaybe)

data Hand = Rock | Paper | Scissors deriving (Show, Eq, Generic, Uniform)

data Winner = Computer | Human | Draw deriving (Show, Eq)

data InputError = Unexpected | EOF deriving (Show, Eq)

data Score = Score Int Int deriving (Eq)

instance Show Score where
  show (Score h c) =
    "You: "
      <> show h
      <> " - Computer: "
      <> show c

checkWinner :: Hand -> Hand -> Winner
checkWinner Rock Paper = Human
checkWinner Paper Rock = Computer
checkWinner Paper Scissors = Human
checkWinner Scissors Paper = Computer
checkWinner Scissors Rock = Human
checkWinner Rock Scissors = Computer
checkWinner _ _ = Draw

getHand :: IO (Either InputError Hand)
getHand = do
  input <-
    runInputT
      defaultSettings
      (getInputLine "Choose your hand: (1) Rock (2) Paper (3) Scissors: ")
  case input of
    Nothing -> pure $ Left EOF
    Just n -> case readMaybe n :: Maybe Int of
      Just 1 -> pure (Right Rock)
      Just 2 -> pure (Right Paper)
      Just 3 -> pure (Right Scissors)
      _ -> pure (Left Unexpected)

main :: IO ()
main = do
  score <- gameLoop (Score 0 0)
  putStrLn $ "Final score: " <> show score
  putStrLn "Bye!"

updateScore :: Winner -> Score -> Score
updateScore Human (Score h c) = (Score (succ h) c)
updateScore Computer (Score h c) = (Score h (succ c))
updateScore Draw score = score

finishRound :: Score -> Winner -> IO Score
finishRound newScore = \case
  Human -> putStrLn "You won!" >> newRound
  Computer -> putStrLn "You lost. :(" >> newRound
  Draw -> putStrLn "Draw." >> newRound
  where
    newRound = do
      putStrLn $ "Current score: " <> show newScore
      gameLoop newScore

gameLoop :: Score -> IO Score
gameLoop score = do
  hand <- getHand
  case hand of
    Left EOF -> pure score
    Left Unexpected -> do
      putStrLn "Please enter a valid option"
      gameLoop score
    Right hand' -> do
      computerHand <- uniformM globalStdGen

      putStrLn $ "You picked: " <> show hand'
      putStrLn $ "Computer picked: " <> show computerHand

      let winner = checkWinner computerHand hand'
      let newScore = updateScore winner score

      finishRound newScore winner
