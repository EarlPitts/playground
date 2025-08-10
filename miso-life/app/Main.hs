-----------------------------------------------------------------------------
{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

-----------------------------------------------------------------------------
module Main where

-----------------------------------------------------------------------------

-----------------------------------------------------------------------------

import Grid
import Life
import Miso hiding (media_)
import Miso.Lens
import Miso.String
import Miso.Style hiding (ms)
import Prelude hiding (rem, unlines)

-----------------------------------------------------------------------------

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

-----------------------------------------------------------------------------
#ifdef WASM
foreign export javascript "hs_start" main :: IO ()
#endif
-----------------------------------------------------------------------------
main :: IO ()
main = run $ startComponent app

-----------------------------------------------------------------------------
app :: App Model Action
app =
  (component (Model $ initBoard size) updateModel viewModel)
    { events = pointerEvents,
      styles = [Sheet (sheet size)]
    }
  where
    size = 15

-----------------------------------------------------------------------------
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

-----------------------------------------------------------------------------

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
        (uncurry cellView <$> makeCells grid)
    ]

cellView :: Coord -> State -> View Model Action
cellView c Alive = div_ [class_ "grid-cell-on", onPointerDown (const $ Clicked c)] []
cellView c Dead = div_ [class_ "grid-cell-off", onPointerDown (const $ Clicked c)] []

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
