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
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C8
import qualified Miso.String as T

data Action
  = Run
  | Step
  | Reset
  | UpdateInput MisoString
  deriving (Show, Eq)

initState =
  VM
    { tape = fromList (replicate 10 0),
      program = fromList $ parse "[-]>,[>,]<[.<]",
      -- input = [72,101,108,108,111,32,87,111,114,108,100,33,0],
      input = [],
      output = []
    }

app :: App VM Action
app =
  (component initState updateModel viewModel)
    { styles = [Sheet sheet]
    }

updateModel :: Action -> Effect parent props VM Action
updateModel = \case
  Run -> this %= exec'
  Step -> this %= execStep
  Reset -> this .= initState
  UpdateInput i ->
    this %= (\vm -> vm {input =  BS.unpack (C8.pack (fromMisoString i))})

viewTape :: Zipper Cell -> View VM Action
viewTape (Zip begin (curr : end)) =
  H.div_
    [] $
    [ H.h2_ [] ["Tape"],
      H.div_ [P.class_ "zipper-container"] $
        fmap (viewCell []) (reverse begin)
        <> [viewCell [P.class_ "zipper-selected"] curr]
        <> fmap (viewCell []) end
    ]

viewCell :: [Attribute Action] -> Cell -> View VM Action
viewCell attrs c = H.div_ attrs [text (ms (show c))]

viewProgram :: Zipper Instr -> View VM Action
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

viewInstr :: [Attribute Action] -> Instr -> View VM Action
viewInstr attrs c = H.div_ attrs [text (ms (show c))]

viewInput :: [Word8] -> View VM Action
viewInput ws =
  H.div_ []
    [ H.h2_ [] ["Input"],
      input_ [ type_ "text"
            , value_ (ms $ C8.unpack (BS.pack ws))
            , onInput UpdateInput
            ]
    ]

viewOutput :: [Word8] -> View VM Action
viewOutput ws =
  H.div_ []
    [ H.h2_ [] ["Output"],
      text $ ms $ C8.unpack (BS.pack (reverse ws))
    ]

viewModel :: props -> VM -> View VM Action
viewModel _ VM {..} =
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
