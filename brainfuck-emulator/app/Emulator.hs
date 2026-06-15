{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Emulator where

import           Miso
import           Miso.Html.Element as H
import           Miso.Html.Event as E
import           Miso.Html.Property as P
import           Miso.Types
import           Miso.Lens
import Style
import BrainFuck
import Data.List.Zipper
import Data.Word

data Action
  = AddOne
  | SubtractOne
  | SayHelloWorld
  deriving (Show, Eq)

initState = VM {
  tape = fromList [0]
  , program = fromList $ parse "<><>>"
  , output = []
  , input = []
}

app :: App VM Action
app = (component initState updateModel viewModel)
  { styles = [ Sheet sheet ]
  }

updateModel :: Action -> Effect parent props VM Action
updateModel = undefined
  -- AddOne ->
  --   this += 2
  -- SubtractOne ->
  --   this -= 1
  -- SayHelloWorld ->
  --   io_ (consoleLog "Hello World!")


viewTape :: Zipper Cell -> View VM Action
viewTape (Zip begin (curr:end)) = H.div_
    [ P.class_ "counter-display" ]
    [
    H.h2_
      [P.class_ "counter-display"]
      ["Tape"],
      text
        (ms $ show (reverse begin) <> "> " <> show curr <> " <"  <> show end )

    ]

viewProgram :: Zipper Instr -> View VM Action
viewProgram (Zip begin (curr:end)) = H.div_
    [ P.class_ "counter-display" ]
    [
    H.h2_
      [P.class_ "counter-display"]
      ["Tape"],
      text
        (ms $ show (reverse begin) <> "> " <> show curr <> " <"  <> show end )

    ]

viewInput :: [Word8] -> View VM Action
viewInput ws = H.div_
    [ P.class_ "counter-display" ]
    [
    H.h2_
      [P.class_ "counter-display"]
      ["Input"],
      text (ms $ show ws)
    ]

viewOutput :: [Word8] -> View VM Action
viewOutput ws = H.div_
    [ P.class_ "counter-display" ]
    [
    H.h2_
      [P.class_ "counter-display"]
      ["Output"],
      text (ms $ show ws)
    ]
  

viewModel :: props -> VM -> View VM Action
viewModel _ VM{..} = H.div_
  [ P.class_ "counter-container" ]
  [ H.h1_
    [ P.class_ "counter-title" ]
    [ "BF Emulator" ]
  , viewTape tape
  , viewProgram program
  , viewInput input
  , viewOutput output
  -- , H.div_
  --   [ P.class_ "buttons-container"
  --   ]
  --   [ H.button_
  --     [ E.onClick SubtractOne
  --     , P.class_ "decrement-btn"
  --     ] [text "-"]
  --   , H.button_
  --     [ E.onClick AddOne
  --     , P.class_ "increment-btn"
  --     ] [text "+"]
  --   ]
  ]
