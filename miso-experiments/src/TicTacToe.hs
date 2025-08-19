{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module TicTacToe where

import Data.Foldable (asum)
import Data.List (transpose, (!!))
import Data.Map ()
import Data.Maybe (isJust, isNothing)
import Miso hiding (style_)
import Miso.String (MisoString, ms)
import Miso.Style hiding (ms)

ticTacToeApp :: App Model Action
ticTacToeApp = component initModel updateModel viewModel
  where
    -- initialAction = None
    events = defaultEvents
    -- subs          = []
    mountPoint = Nothing
    logLevel = Off

initModel :: Model
initModel =
  Model
    { grid = emptyGrid,
      currentPlayer = X,
      winner = Nothing,
      names = PlayerNames "" "",
      isRunning = False
    }

data Player
  = X
  | O
  deriving (Show, Eq)

type Grid = [[Maybe Player]]

emptyGrid, aGrid :: Grid
emptyGrid = replicate 3 (replicate 3 Nothing)
aGrid =
  [ [Just X, Nothing, Nothing],
    [Nothing, Just O, Nothing],
    replicate 3 Nothing
  ]

data PlayerNames = PlayerNames MisoString MisoString deriving (Show, Eq)

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

data Model = Model
  { grid :: Grid,
    currentPlayer :: Player,
    winner :: Maybe Player,
    names :: PlayerNames,
    isRunning :: Bool
  }
  deriving (Show, Eq)

data Action
  = ClickPlayer Int Int
  | NewGame
  | SetNames PlayerNames
  deriving (Show, Eq)

updateModel :: Action -> Transition Model Action
updateModel (ClickPlayer rowId colId) = do
  m@(Model grid player _ names _) <- get
  let newGrid = (mark rowId colId player grid)
  let winner = hasWinner newGrid
  if (isJust winner)
    then put (Model newGrid player winner names False)
    else
      let isRunning = not (isFinished m)
       in put (Model newGrid (nextPlayer player) Nothing names isRunning)
updateModel NewGame = do
  (Model _ _ _ names _) <- get
  put $ Model emptyGrid X Nothing names True
updateModel (SetNames names) = do
  (Model grid currentPlayer winner _ _) <- get
  put $ Model grid currentPlayer winner names False

bootstrapUrl :: MisoString
bootstrapUrl = "https://stackpath.bootstrapcdn.com/bootstrap/4.5.2/css/bootstrap.min.css"

mark :: Int -> Int -> Player -> Grid -> Grid
mark r c sq g = newGrid
  where
    row = g !! r
    newRow = take c row ++ [Just sq] ++ drop (c + 1) row
    newGrid = take r g ++ [newRow] ++ drop (r + 1) g

isFinished :: Model -> Bool
isFinished (Model _ _ (Just _) _ _) = True
isFinished (Model grid _ _ _ _) = all id $ fmap (all isJust) grid

viewModel :: Model -> View Model Action
viewModel m =
  div_
    [class_ "container"]
    [ headerView,
      newGameView m,
      contentView m,
      -- , statsView m
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
newGameView (Model _ _ _ (PlayerNames p1 p2) isRunning) =
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
contentView (Model grid currentPlayer winner names isRunning) =
  div_
    [style_ [margin "20px"]]
    [ gridView grid currentPlayer names isRunning,
      alertView winner
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

alertView :: Maybe Player -> View Model Action
alertView winner =
  div_
    [ class_ "alert alert-warning",
      style_ [textAlign "center"],
      hidden_ $ isNothing winner
    ]
    [ h4_
        [class_ "alert-heading"]
        [text (ms (show winner))]
    ]

fakeStats :: [MisoString]
fakeStats =
  [ "A - B, won by A in 3 moves",
    "Quijote - Sancho, won by Sancho in 5 moves"
  ]

statsView :: Model -> View Model Action
statsView _ =
  div_
    [ class_ "row justify-content-around align-items-center",
      style_ [marginBottom "20px"]
    ]
    [ ul_
        [class_ "list-group"]
        ( flip map fakeStats $ \elt ->
            ul_ [class_ "list-group-item"] [text elt]
        )
    ]
