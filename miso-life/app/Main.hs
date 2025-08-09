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
  deriving (Show, Eq)

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
  (component (Model $ glider size) updateModel viewModel)
    { events = pointerEvents,
      styles = [Sheet (sheet size)]
    }
  where
    size = 15

-----------------------------------------------------------------------------
updateModel :: Action -> Transition Model Action
updateModel Step = do
  value %= step
  io_ $ consoleLog "changed"

-----------------------------------------------------------------------------

viewModel :: Model -> View Model Action
viewModel (Model grid) =
  div_
    [class_ "grid-container"]
    [ div_
        []
        [button_ [onPointerDown (const Step)] [text "Step"]],
      div_
        [class_ "container"]
        [cellView c | r <- toLists grid, c <- r]
    ]

cellView :: State -> View Model Action
cellView Alive = div_ [class_ "grid-cell-on"] []
cellView Dead = div_ [class_ "grid-cell-off"] []

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
