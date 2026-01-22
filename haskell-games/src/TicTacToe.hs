module TicTacToe where

import Data.List (intercalate, isInfixOf)
import Data.List.Split (chunksOf)
import System.Console.Haskeline
import System.Random
import Text.Read (readMaybe)

data Tile = X | O | Empty deriving (Eq)

data Board = Board [(Int, Tile)] deriving (Eq)

instance Show Tile where
  show X = "X"
  show O = "O"
  show Empty = " "

instance Show Board where
  show (Board ts) =
    concatMap
      (\line -> intercalate " | " (fmap showField line) <> "\n")
      (chunksOf 3 ts)
    where
      showField (n, Empty) = show n
      showField (_, tile) = show tile

data InputError = EOF | InvalidMove | Unexpected deriving (Show, Eq)

getMove :: [Int] -> IO (Either InputError Int)
getMove available = do
  input <-
    runInputT
      defaultSettings
      (getInputLine "Next move: ")
  case input of
    Nothing -> pure $ Left EOF
    Just n -> case readMaybe n :: Maybe Int of
      Just n -> if n `elem` available then pure $ Right n else pure $ Left InvalidMove
      Nothing -> pure (Left Unexpected)

initBoard :: Board
initBoard = Board $ zip [1 .. 9] (repeat Empty)

updateState :: Tile -> Int -> Board -> Board
updateState tile n (Board ts) =
  Board $ fmap (\(n', t') -> if n == n' then (n, tile) else (n', t')) ts

getAvailable :: Board -> [Int]
getAvailable (Board ts) =
  fmap fst $ filter (\(_, tile) -> tile == Empty) ts

computerStep :: Board -> IO Board
computerStep b = do
  let available = getAvailable b
  n <- randomRIO (0, pred $ length available)
  let choice = available !! n
  putStrLn $ "Computer's move: " <> show choice
  pure $ updateState O choice b

checkWin :: Board -> Tile -> Bool
checkWin b t =
  any (`isInfixOf` nums) (horizontal <> vertical <> across)
  where
    horizontal = [[1, 2, 3], [4, 5, 6], [7, 8, 9]]
    vertical = [[1, 4, 7], [2, 5, 8], [3, 6, 9]]
    across = [[1, 5, 9], [3, 5, 7]]
    nums = getTiles b t

getTiles :: Board -> Tile -> [Int]
getTiles (Board b) t = fmap fst (filter (\(_, tile) -> tile == t) b)

gameLoop :: Board -> IO ()
gameLoop board = do
  let available = getAvailable board
  input <- getMove available
  case input of
    Left _ -> do
      putStrLn "Please enter a valid number"
      gameLoop board
    Right n -> do
      let newState = updateState X n board
      putStrLn "---- You ----\n"
      print newState
      if checkWin newState X
        then do
          putStrLn "You won!"
        else do
          newerState <- computerStep newState
          putStrLn "---- Computer ----\n"
          print newerState
          if checkWin newerState O
            then do
              putStrLn "Computer won"
            else
              gameLoop newerState

main :: IO ()
main = do
  print initBoard
  gameLoop initBoard
