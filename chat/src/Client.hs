module Client where

import Brick
import qualified Brick.AttrMap as A
import qualified Brick.Main as M
import qualified Brick.Types as T
import Brick.Widgets.Border
import Brick.Widgets.Border.Style
import Brick.Widgets.Edit
import Brick.Widgets.List as L
import Data.List (head)
import Data.Vector (Vector (..))
import qualified Data.Vector as V
import qualified Graphics.Vty as V
import Protolude hiding (head)

main :: IO ()
main = void $ M.defaultMain theApp (AppState dummyMsgs emptyEditor)

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

drawUI :: AppState -> [Widget Int]
drawUI AppState{..} = [vBox [history sMessages, textBox sEditor]]

history :: Vector Message -> Widget Int
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
            (L.list 1 messages 1)

textBox :: Editor Text Int -> Widget Int
textBox =
  borderWithLabel (str "Reply")
    . renderEditor (\t -> txt $ head t) True

appEvent :: BrickEvent Int e -> EventM Int AppState ()
appEvent (T.VtyEvent e) = case e of
  V.EvKey (V.KEsc) [] -> M.halt
  V.EvKey (V.KEnter) [] -> do
    editorState <- gets sEditor
    let msg = head $ getEditContents editorState
    modify (\s -> s{sEditor = emptyEditor})
  _ -> do
    editorState <- gets sEditor
    newEditorState <- nestEventM' editorState $ handleEditorEvent (VtyEvent e)
    modify (\s -> s{sEditor = newEditorState})
appEvent _ = pure ()

data Message
  = Other
      { mUsername :: Text
      , mText :: Text
      }
  | Own {mText :: Text}
  deriving (Show, Eq)

data AppState = AppState
  { sMessages :: Vector Message
  , sEditor :: Editor Text Int
  }

theApp :: M.App AppState e Int
theApp =
  M.App
    { M.appDraw = drawUI
    , M.appChooseCursor = M.showFirstCursor
    , M.appHandleEvent = appEvent
    , M.appStartEvent = pure ()
    , M.appAttrMap = const $ A.attrMap V.defAttr []
    }
