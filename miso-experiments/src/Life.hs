{-# LANGUAGE OverloadedStrings #-}

module Life where

import Grid
import Control.Comonad
import Miso hiding (focus, Coord)
import Miso.Lens
import Miso.Html.Event
import Miso.Html.Property
import Miso.Html.Element
import Miso.String (ToMisoString)
import Miso.CSS hiding (ms, filter)


type Board = Grid State

data State = Dead | Alive deriving (Show, Eq)

step :: Grid State -> Grid State
step = extend rule

rule :: Grid State -> State
rule g =
  if isAlive g
  then (if numOfNeighbors < 2
        then Dead
        else (if numOfNeighbors > 3 then Dead else Alive))
  else (if numOfNeighbors == 3 then Alive else Dead)
    where
      numOfNeighbors = length $ filter isAlive (adjacent g)
      isAlive cell = Alive == (focus cell)

data Coord = Coord Int Int deriving Show

makeCells :: Grid State -> [(Coord, State)]
makeCells g = do
  (i, row) <- zip [0..] (toLists g)
  (j, state) <- zip [0..] row
  return (Coord i j, state)

toggle :: State -> State
toggle Alive = Dead
toggle Dead = Alive

toggleCell :: Coord -> Board -> Board
toggleCell (Coord r c) b = maybe b id (safeUpdate r c toggle b)

glider :: Int -> Board
glider size = fromLists $ [line1, line2, line3] <> (Prelude.replicate (size - 3) (Prelude.replicate size Dead))
  where
    line1 = [Dead, Alive] <> replicate 13 Dead
    line2 = [Dead, Dead, Alive] <> replicate 12 Dead
    line3 = [Alive, Alive, Alive] <> replicate 12 Dead

initBoard :: Int -> Board
initBoard size = fromLists (Prelude.replicate size (Prelude.replicate size Dead))

life :: App Model Action
life =
  (component (Model $ initBoard size) updateModel viewModel)
    { events = defaultEvents <> pointerEvents,
      styles = [Sheet (sheet size)]
    }
  where
    size = 15

newtype Model = Model {_value :: Grid State}
  deriving (Show, Eq)

-----------------------------------------------------------------------------
instance ToMisoString Model where
  toMisoString (Model v) = toMisoString (show v)

-----------------------------------------------------------------------------
value :: Lens Model (Grid State)
value = lens _value $ \m v -> m {_value = v}

-----------------------------------------------------------------------------
data Action
  = Step
  | Run
  | Clicked Coord
  deriving (Show)

updateModel :: Action -> Transition Model Action
updateModel Step = do
  value %= step
  io_ $ consoleLog "step"
updateModel Run = do
  value %= step
  io $ pure Run
updateModel (Clicked coord@(Coord r c)) = do
  value %= toggleCell coord
  io_ $ consoleLog ("changed row " <> (ms r) <> " column " <> (ms c))

viewModel :: Model -> View Model Action
viewModel (Model grid) =
  div_
    [class_ "grid-container"]
    [ div_
        []
        [button_ [onClick Step] [text "Step"]],
      div_
        []
        [button_ [onClick Run] [text "Run"]],
      div_
        [class_ "container"]
        (uncurry cellView <$> makeCells grid)
    ]

cellView :: Coord -> State -> View Model Action
cellView c Alive = div_ [class_ "grid-cell-on", onClick $ Clicked c] []
cellView c Dead = div_ [class_ "grid-cell-off", onClick $ Clicked c] []

sheet :: Int -> StyleSheet
sheet size =
  sheet_
    [ selector_
        ".grid-container"
        [ display "grid",
          justifyContent "center",
          alignItems "center",
          height "100vh"
        ],
      selector_
        ".container"
        [ display "grid",
          gridTemplateColumns (ms $ "repeat(" <> (show size) <> ", 40px)"),
          gridTemplateRows (ms $ "repeat(" <> (show size) <> ", 40px)"),
          gap "0"
        ],
      selector_
        ".grid-cell-off"
        [ border "1px solid #333",
          height "40px",
          width "40px"
        ],
      selector_
        ".grid-cell-on"
        [ border "1px solid #333",
          height "40px",
          width "40px",
          background "black"
        ]
    ]
