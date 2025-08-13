{-# LANGUAGE OverloadedStrings #-}

module Snake where

import Grid
import Data.Maybe
import Control.Comonad
import Miso hiding (media_, focus, update, set)
import Miso.Lens hiding (set)
import Miso.String (ms, toMisoString, ToMisoString)
import Miso.Style hiding (ms, filter, position)
import Debug.Trace

type Board = Grid Tile

data Dir = U | D| L | R deriving (Show, Eq)
data Tile = Head Dir Dir | Tail Dir | Empty deriving (Show, Eq)

step :: Grid Tile -> Grid Tile
step = undefined

shrink :: Grid Tile -> Grid Tile
shrink g = case focus g of
  (Tail U) -> case focus <$> safeUp g of
    Nothing -> set Empty g
    Just Empty -> set Empty g
    Just _ -> shrink (moveUp g)
  (Tail D) -> case focus <$> safeDown g of
    Nothing -> set Empty g
    Just Empty -> set Empty g
    Just _ -> shrink (moveDown g)
  (Tail R) -> case focus <$> safeRight g of
    Nothing -> set Empty g
    Just Empty -> set Empty g
    Just _ -> shrink (moveRight g)
  (Tail L) -> case focus <$> safeLeft g of
    Nothing -> set Empty g
    Just Empty -> set Empty g
    Just _ -> shrink (moveLeft g)
  _ -> g

expand :: Grid Tile -> Grid Tile
expand g = case f of
            U -> set (Head U D) $ moveUp (set (Tail b) g)
            D -> set (Head D U) $ moveDown (set (Tail b) g)
            L -> set (Head L R) $ moveLeft (set (Tail b) g)
            R -> set (Head R L) $ moveRight (set (Tail b) g)
  where
    (Head f b) = focus g

makeCells :: Grid Tile -> [Tile]
makeCells g = do
  row <- (toLists g)
  state <- row
  return state

initBoard :: Int -> Board
initBoard size = set (Head D U) $ moveDown (set (Tail U) $ moveDown (set (Tail U) middle))
  where
    empty = emptyBoard size
    middle = fromMaybe empty $ safeRightN (div size 2) empty >>= safeDownN (div size 2)

emptyBoard :: Int -> Board
emptyBoard size = fromLists (Prelude.replicate size (Prelude.replicate size Empty))

snake :: App Model Action
snake =
  (component (Model $ initBoard size) updateModel viewModel)
    { events = pointerEvents,
      styles = [Sheet (sheet size)]
    }
  where
    size = 15

newtype Model = Model {_value :: Grid Tile}
  deriving (Show, Eq)

-----------------------------------------------------------------------------
instance ToMisoString Model where
  toMisoString (Model v) = toMisoString (show v)

-----------------------------------------------------------------------------
value :: Lens Model (Grid Tile)
value = lens _value $ \m v -> m {_value = v}

-----------------------------------------------------------------------------
data Action
  = Step
  | Run
  deriving (Show)

updateModel :: Action -> Transition Model Action
updateModel Step = do
  -- value %= step
  (Model grid) <- get
  io_ $ consoleLog (ms (show (position grid)))
  io_ $ consoleLog "step"
updateModel Run = do
  value %= step
  io $ pure Run

viewModel :: Model -> View Model Action
viewModel (Model grid) =
  div_
    [class_ "grid-container"]
    [ div_
        []
        [button_ [onPointerDown (const Step)] [text "Step"]],
      div_
        []
        [button_ [onPointerDown (const Run)] [text "Run"]],
      div_
        [class_ "container"]
        (cellView <$> makeCells grid)
    ]

cellView :: Tile -> View Model Action
cellView Empty = div_ [class_ "grid-cell-off"] []
cellView (Head dir _) = div_ [class_ "grid-cell-on"] [ text (ms (show dir)) ]
cellView (Tail dir) = div_ [class_ "grid-cell-on"] [ text (ms (show dir)) ]

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
          background "black",
          color white,
          textAlign "center"
        ]
    ]
