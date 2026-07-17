module Client where

import Brick
import qualified Brick.AttrMap as A
import Brick.BChan
import qualified Brick.Main as M
import qualified Brick.Types as T
import Brick.Widgets.Border
import Brick.Widgets.Border.Style
import Brick.Widgets.Edit
import Brick.Widgets.List as L
import Data.List (head)
import Data.Vector (Vector (..))
import qualified Data.Vector as V
import Graphics.Vty (defaultConfig)
import qualified Graphics.Vty as V
import Graphics.Vty.Platform.Unix (mkVty)
import Protolude hiding (head)

main :: IO ()
main = do
  chan <- newBChan 10
  let buildVty = mkVty defaultConfig
  initialVty <- buildVty
  void $ M.customMain initialVty buildVty (Just chan) theApp (AppState emptyHistory emptyEditor)

dummyMsgs =
  V.fromList
    [ Own "hello"
    , Other "John" "Hey!"
    , Other "Mike" "Ho!"
    , Own "Whatsup?"
    , Other "John" "The sky!"
    , Own "Haha!"
    ]

emptyEditor = editor 2 (Just 1) ""
emptyHistory = L.list 1 dummyMsgs 1

drawUI :: AppState -> [Widget Int]
drawUI AppState{..} = [vBox [history sHistory, textBox sEditor]]

data Event = NewMessage Text

history :: List Int Message -> Widget Int
history messages =
  joinBorders $
    withBorderStyle unicode $
      borderWithLabel (str "Chat") $
        padLeftRight 1 $
          L.renderList
            ( \_ -> \case
                (Other name msg) -> txt $ name <> ": " <> msg
                (Own msg) -> padLeft Max (txt msg)
            )
            False
            messages

textBox :: Editor Text Int -> Widget Int
textBox =
  borderWithLabel (txt "Reply")
    . renderEditor (\t -> txt $ head t) True

appEvent :: BrickEvent Int Text -> EventM Int AppState ()
appEvent (T.VtyEvent e) = case e of
  V.EvKey (V.KEsc) [] -> M.halt
  V.EvKey (V.KEnter) [] -> do
    editorState <- gets sEditor
    let msg = head $ getEditContents editorState
    modify (\s -> s{sHistory = L.list 1 (V.snoc (listElements $ sHistory s) (Own msg)) 1, sEditor = emptyEditor})
  _ -> do
    editorState <- gets sEditor
    newEditorState <- nestEventM' editorState $ handleEditorEvent (VtyEvent e)
    modify (\s -> s{sEditor = newEditorState})
appEvent (T.AppEvent msg) =
  modify (\s -> s{sHistory = L.list 1 (V.snoc (listElements $ sHistory s) (Other "jani" msg)) 1}) -- TODO
appEvent _ = pure ()

data Message
  = Other
      { mUsername :: Text
      , mText :: Text
      }
  | Own {mText :: Text}
  deriving (Show, Eq)

data AppState = AppState
  { sHistory :: List Int Message
  , sEditor :: Editor Text Int
  }

theApp :: M.App AppState Text Int
theApp =
  M.App
    { M.appDraw = drawUI
    , M.appChooseCursor = M.showFirstCursor
    , M.appHandleEvent = appEvent
    , M.appStartEvent = pure ()
    , M.appAttrMap = const $ A.attrMap V.defAttr []
    }
