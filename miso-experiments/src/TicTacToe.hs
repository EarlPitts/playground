{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}


module TicTacToe where

import GHC.Generics
import Data.Foldable (asum)
import Data.List (transpose)
import Data.Map ()
import Data.Maybe (isJust, isNothing)
import Miso hiding (style_)
import Miso.Lens
import Miso.String (MisoString, ToMisoString, ms, toMisoString)
import Miso.Style hiding (ms)
import Data.Aeson (FromJSON, ToJSON)

ticTacToeApp :: App Model Action
ticTacToeApp = (component initModel updateModel viewModel) {
    initialAction = Just LoadStats
  }

data Model = Model
  { _grid :: Grid,
    _currentPlayer :: Player,
    _winner :: Maybe Player,
    _names :: PlayerNames,
    _isRunning :: Bool,
    _moves :: Moves,
    _stats :: [Stat]
  }
  deriving (Show, Eq)

type Moves = Int

data Stat = Stat PlayerNames Player Moves deriving (Show, Eq, Generic, FromJSON, ToJSON)

instance ToMisoString Stat where
  toMisoString (Stat (PlayerNames p1 p2) player moves) =
    p1 <> " - " <> p2 <> ", won by " <> winner <> " in " <> (ms moves) <> " moves"
    where
      winner = if player == X then p1 else p2

data Player
  = X
  | O
  deriving (Show, Eq, Generic, FromJSON, ToJSON)

type Grid = [[Maybe Player]]

emptyGrid, aGrid :: Grid
emptyGrid = replicate 3 (replicate 3 Nothing)
aGrid =
  [ [Just X, Nothing, Nothing],
    [Nothing, Just O, Nothing],
    replicate 3 Nothing
  ]

data PlayerNames = PlayerNames MisoString MisoString deriving (Show, Eq, Generic, FromJSON, ToJSON)

hasWinner :: Grid -> Maybe Player
hasWinner g =
  asum (map isWinnerRow thingToCheck)
  where
    thingToCheck =
      g
        ++ transpose g
        ++ [ [g !! 0 !! 0, g !! 1 !! 1, g !! 2 !! 2],
             [g !! 0 !! 2, g !! 1 !! 1, g !! 2 !! 0]
           ]
    isWinnerRow :: [Maybe Player] -> Maybe Player
    isWinnerRow row
      | all isJust row,
        all (== head row) row =
          head row
      | otherwise =
          Nothing

nextPlayer :: Player -> Player
nextPlayer X = O
nextPlayer O = X

mark :: Int -> Int -> Player -> Grid -> Grid
mark r c sq g = newGrid
  where
    row = g !! r
    newRow = take c row ++ [Just sq] ++ drop (c + 1) row
    newGrid = take r g ++ [newRow] ++ drop (r + 1) g

isFinished :: Model -> Bool
isFinished (Model _ _ (Just _) _ _ _ _) = True
isFinished (Model grid _ _ _ _ _ _) = all id $ fmap (all isJust) grid

updateStats :: Model -> [Stat]
updateStats (Model _ _ (Just winner) names _ moves stats) = (Stat names winner moves) : stats
updateStats _ = error "Shouldn't happen"

initModel :: Model
initModel =
  Model
    { _grid = emptyGrid,
      _currentPlayer = X,
      _winner = Nothing,
      _names = PlayerNames "" "",
      _isRunning = False,
      _moves = 0,
      _stats = []
    }

gridL :: Lens Model Grid
gridL = lens _grid (\m v -> m {_grid = v})

currentPlayerL :: Lens Model Player
currentPlayerL = lens _currentPlayer (\m v -> m {_currentPlayer = v})

winnerL :: Lens Model (Maybe Player)
winnerL = lens _winner (\m v -> m {_winner = v})

namesL :: Lens Model PlayerNames
namesL = lens _names (\m v -> m {_names = v})

isRunningL :: Lens Model Bool
isRunningL = lens _isRunning (\m v -> m {_isRunning = v})

movesL :: Lens Model Moves
movesL = lens _moves (\m v -> m {_moves = v})

statsL :: Lens Model [Stat]
statsL = lens _stats (\m v -> m {_stats = v})

data Action
  = ClickPlayer Int Int
  | NewGame
  | SetNames PlayerNames
  | LoadStats
  | UpdateStats [Stat]
  | SaveStats [Stat]
  deriving (Show, Eq)

updateModel :: Action -> Transition Model Action
updateModel NewGame = do
  ns <- use namesL
  stats <- use statsL
  put $ initModel {_names = ns, _isRunning = True, _stats = stats}
updateModel (SetNames ns) = do
  namesL .= ns
updateModel (SaveStats stats) =
  io_ $ setLocalStorage "stats" stats
updateModel LoadStats = io $ do
  maybeStats <- getLocalStorage "stats"
  case maybeStats of
    Right stats -> pure $ UpdateStats stats
    Left _ -> pure $ UpdateStats []
updateModel (UpdateStats stats) = statsL .= stats
updateModel (ClickPlayer r c) = do
  Model grid player _ _ _ _ _ <- get
  movesL += 1
  let newGrid = (mark r c player grid)
  let winner = hasWinner newGrid
  if (isJust winner)
    then do
      gridL .= newGrid
      winnerL .= winner
      isRunningL .= False
      m <- get
      let newStats = updateStats m
      statsL .= newStats
      issue $ SaveStats newStats
    else do
      gridL .= newGrid
      currentPlayerL %= nextPlayer
      m <- get
      isRunningL .= not (isFinished m)

bootstrapUrl :: MisoString
bootstrapUrl = "https://stackpath.bootstrapcdn.com/bootstrap/4.5.2/css/bootstrap.min.css"

viewModel :: Model -> View Model Action
viewModel m =
  div_
    [class_ "container"]
    [ headerView,
      newGameView m,
      contentView m,
      statsView m,
      link_
        [ rel_ "stylesheet",
          href_ bootstrapUrl
        ]
    ]

headerView :: View Model Action
headerView =
  nav_
    [class_ "navbar navbar-dark bg-dark"]
    [ h2_
        [class_ "bd-title text-light"]
        [ text "Tic-Tac-Toe ",
          span_
            [class_ "badge badge-warning"]
            [text "in miso!"]
        ]
    ]

newGameView :: Model -> View Model Action
newGameView (Model _ _ _ (PlayerNames p1 p2) isRunning _ _) =
  nav_
    [class_ "navbar navbar-light bg-light"]
    [ form
        [class_ "form-inline", disabled_ isRunning]
        [ input_
            [ class_ "form-control mr-sm-2",
              type_ "text",
              value_ (ms p1),
              onChange $ SetNames . (flip PlayerNames) p2,
              placeholder_ "Player 1"
            ],
          -- , select_ [ class_       "custom-select"
          --           , style_ [("margin-right", "15px")] ]
          --           (flip map ["A", "B"] $ \option ->
          --              option_ [ ] [ text option])
          input_
            [ class_ "form-control mr-sm-2",
              type_ "text",
              value_ p2,
              onChange $ SetNames . PlayerNames p1,
              placeholder_ "Player 2"
            ],
          -- , select_ [ class_       "custom-select"
          --           , style_ [("margin-right", "15px")] ]
          --           (flip map ["A", "B"] $ \option ->
          --              option_ [ ] [ text option])
          button_
            [ class_ "btn btn-outline-warning",
              type_ "button",
              onClick NewGame,
              disabled_ $ isRunning
            ]
            [text "New game"]
        ]
    ]

contentView :: Model -> View Model Action
contentView (Model grid currentPlayer winner names isRunning _ _) =
  div_
    [style_ [margin "20px"]]
    [ gridView grid currentPlayer names isRunning,
      alertView winner names
    ]

gridView :: Grid -> Player -> PlayerNames -> Bool -> View Model Action
gridView grid currentPlayer (PlayerNames p1 p2) isRunning =
  div_
    [style_ [margin "20px"]]
    [ div_
        [class_ "row justify-content-around align-items-center"]
        [ h3_ (if currentPlayer == X then [class_ "badge badge-warning"] else []) [text p1],
          div_
            [style_ [display "inline-block"]]
            [ div_
                [ style_
                    [ display "grid",
                      gridTemplateRows "1fr 1fr 1fr",
                      gridTemplateColumns "1fr 1fr 1fr",
                      gap "2px"
                    ]
                ]
                ( flip concatMap (zip [0 ..] grid) $ \(rowId, row) ->
                    flip map (zip [0 ..] row) $ \(colId, sq) ->
                      cell rowId colId sq isRunning
                )
            ],
          h3_ (if currentPlayer == O then [class_ "badge badge-warning"] else []) [text p2]
        ]
    ]
  where
    cell :: Int -> Int -> Maybe Player -> Bool -> View Model Action
    cell rowId colId square isRunning =
      div_
        [style_ [width "100px", height "100px"]]
        [ button_
            [ type_ "button",
              style_
                [ width "100%",
                  height "100%",
                  fontSize "xxx-large"
                ],
              class_ "btn btn-outline-secondary",
              onClick (ClickPlayer rowId colId),
              disabled_ $ (not isRunning) || isJust square
            ]
            [text (maybe "" (\sq -> if sq == X then "X" else "O") square)]
        ]

alertView :: Maybe Player -> PlayerNames -> View Model Action
alertView winner (PlayerNames p1 p2) =
  div_
    [ class_ "alert alert-warning",
      style_ [textAlign "center"],
      hidden_ $ isNothing winner
    ]
    [ h4_
        [class_ "alert-heading"]
        [ text $
            ( case winner of
                Just X -> p1
                Just O -> p2
                _ -> ""
            )
              <> " won!"
        ]
    ]

fakeStats :: [MisoString]
fakeStats =
  [ "A - B, won by A in 3 moves",
    "Quijote - Sancho, won by Sancho in 5 moves"
  ]

statsView :: Model -> View Model Action
statsView m =
  div_
    [ class_ "row justify-content-around align-items-center",
      style_ [marginBottom "20px"]
    ]
    [ ul_
        [class_ "list-group"]
        ( flip map (_stats m) $ \elt ->
            ul_ [class_ "list-group-item"] [text (ms elt)]
        )
    ]
