{-# LANGUAGE OverloadedStrings #-}

module Snake where

import Grid
import Data.Maybe
import Control.Comonad
import Miso hiding (media_, focus, update, set, Off)
import Miso.Lens hiding (set)
import Miso.String (ms, toMisoString, ToMisoString)
import Miso.Style hiding (ms, filter, position)
import Debug.Trace
import Control.Monad

type Board = [[Position]]

data Dir = U | D | L | R deriving (Show, Eq)

data Snake = Snake Dir [Position] deriving (Show, Eq)
-- data Fruits = Fruits [Position]
data Tile = On | Off

type Size = Int

board :: Snake -> Position -> Tile
board (Snake _ ps) p = if p `elem` ps then On else Off

step :: Snake -> Snake
step (Snake d (h : t)) = Snake d ((move h d) : h : init t)

move :: Position -> Dir -> Position
move (Position r c) U = Position (r - 1) c
move (Position r c) D = Position (r + 1) c
move (Position r c) L = Position r (c - 1)
move (Position r c) R = Position r (c + 1)

initModel :: Int -> Model
initModel size = Model size (Snake R [Position (mid + 1) mid, Position mid mid, Position (mid-1) mid]) False
  where
    mid = div size 2

snakeMain :: App Model Action
snakeMain =
  (component (initModel size) updateModel viewModel)
    { events = pointerEvents,
      subs = [arrowsSub GetArrows],
      styles = [Sheet (sheet size)]
    }
  where
    size = 15

data Model = Model { boardSize :: Size, snake :: Snake , running :: Bool }
  deriving (Show, Eq)

-----------------------------------------------------------------------------
instance ToMisoString Model where
  toMisoString (Model _ s _) = toMisoString (show s)

-----------------------------------------------------------------------------
-- value :: Lens Model (Grid Tile)
-- value = lens _value $ \m v -> m {_value = v}

-----------------------------------------------------------------------------
data Action
  = Step
  | Run
  | GetArrows Arrows
  deriving (Show)

updateModel :: Action -> Transition Model Action
updateModel Step = do
  io_ $ consoleLog "step"
  -- value %= step
  modify (\(Model size snake _) -> Model size (step snake) True)
  get >>= (\(Model _ s _) -> io_ $ consoleLog (ms (show s)))
updateModel Run = do
  -- modify (\(Model size snake _) -> Model size (step snake) True)
  -- value %= step
  batch [pure Step, pure Run]
  -- io $ pure Run
-- updateModel (ChangeDir d) =
--   modify (\(Model size (Snake _ ps)) -> Model size (Snake d ps))
updateModel (GetArrows a) =
  case (getDir a) of
    Just dir -> do
      (Model size (Snake _ ps) running) <- get
      if running
      then modify (\(Model size (Snake _ ps) _) -> Model size (Snake dir ps) True)
      else modify (\(Model size (Snake _ ps) _) -> Model size (Snake dir ps) True) >> (io $ pure Run)
      -- io $ pure (ChangeDir dir)
    Nothing -> pure ()
  
getDir :: Arrows -> Maybe Dir
getDir (Arrows 1 0) = Just R
getDir (Arrows (-1) 0) = Just L
getDir (Arrows 0 1) = Just U
getDir (Arrows 0 (-1)) = Just D
getDir _ = Nothing

positions :: Size -> [Position]
positions s = [ Position r c | r <- [0..s-1], c <- [0..s-1] ]

viewModel :: Model -> View Model Action
viewModel (Model size snake _) =
  div_
    [class_ "grid-container"]
    [
    -- [ div_
    --     []
    --     [button_ [onPointerDown (const Step)] [text "Step"]],
    --   div_
    --     []
    --     [button_ [onPointerDown (const Run)] [text "Run"]],
      div_
        [class_ "container"]
        (cellView <$> ((board snake) <$> positions size))
    ]

cellView :: Tile -> View Model Action
cellView Off = div_ [class_ "grid-cell-off"] []
cellView On = div_ [class_ "grid-cell-on"] []

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
