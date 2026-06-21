{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Emulator where

import BrainFuck
import Data.List.Zipper
import Data.Word
import Miso
import Miso.Html.Element as H
import Miso.Html.Event as E
import Miso.Html.Property as P
import Miso.Lens
import Miso.Types
import Style
import qualified Miso.String as T

data Action
  = Run
  | Step
  | Reset
  | UpdateInput MisoString
  | UpdateProgram MisoString
  deriving (Show, Eq)

data Model = Model {
    _vm :: VM
  , _started :: Bool
} deriving (Show, Eq)

vmL :: Lens Model VM
vmL = lens _vm $ \m v -> m {_vm = v}

startedL :: Lens Model Bool
startedL = lens _started $ \m v -> m {_started = v}

initState :: Model
initState = Model {
    _vm = mkVM "[-]>,[>,]<[.<]" "Hello World!"
  , _started = False
}

app :: App Model Action
app = (component initState updateModel viewModel)
    { styles = [Sheet sheet] }

updateModel :: Action -> Effect parent props Model Action
updateModel = \case
  Run -> vmL %= exec
  Step -> vmL %= execStep
  Reset -> this .= initState
  UpdateInput i -> vmL %= (\vm -> vm {input = toWords (fromMisoString i)})
  -- UpdateProgram p -> vmL %= (\vm -> vm {program = fromList $ parse (fromMisoString p)})

viewTape :: Zipper Cell -> View Model Action
viewTape (Zip begin (curr : end)) =
  H.div_
    [] $
    [ H.h2_ [] ["Tape"],
      H.div_ [P.class_ "zipper-container"] $
        fmap (viewCell []) (reverse begin)
        <> [viewCell [P.class_ "zipper-selected"] curr]
        <> fmap (viewCell []) end
    ]

viewCell :: [Attribute Action] -> Cell -> View Model Action
viewCell attrs c = H.div_ attrs [text (ms (show c))]

viewProgram :: Zipper Instr -> View Model Action
viewProgram (Zip [] []) =
  H.div_
    []
    [ H.h2_ [] ["Program"],
      input_ [ type_ "text"
            , value_ ""
            , onInput UpdateProgram
            ]
    ]
viewProgram (Zip begin (curr : end)) =
  H.div_
    []
    [ H.h2_ [] ["Program"],
      H.div_ [P.class_ "zipper-container"] $
        fmap (viewInstr []) (reverse begin)
        <> [viewInstr [P.class_ "zipper-selected"] curr]
        <> fmap (viewInstr []) end
    ]
viewProgram (Zip begin end) =
  H.div_
    []
    [ H.h2_ [] ["Program"],
      H.div_ [P.class_ "zipper-container"] $
        fmap (viewInstr []) (reverse begin)
        <> fmap (viewInstr []) end
    ]

viewInstr :: [Attribute Action] -> Instr -> View Model Action
viewInstr attrs c = H.div_ attrs [text (ms (show c))]

viewInput :: [Word8] -> View Model Action
viewInput ws =
  H.div_ []
    [ H.h2_ [] ["Input"],
      input_ [ type_ "text"
            , value_ (ms $ fromWords ws)
            , onInput UpdateInput
            ]
    ]

viewOutput :: [Word8] -> View Model Action
viewOutput ws =
  H.div_ []
    [ H.h2_ [] ["Output"],
      text $ ms (fromWords (reverse ws))
    ]

viewModel :: props -> Model -> View Model Action
viewModel _ (Model VM {..} started) =
  H.div_ []
    [ H.h1_ [] ["BF Emulator"]
    , H.button_
        [ E.onClick Run
        ] [text "Run"]
    , H.button_
        [ E.onClick Step
        ] [text "Step"]
    , H.button_
        [ E.onClick Reset
        ] [text "Reset"]
    , viewTape tape
    , viewProgram program
    , viewInput input
    , viewOutput output
    ]
