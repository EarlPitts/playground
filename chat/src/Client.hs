module Client where

import Brick
import qualified Brick.AttrMap as A
import Brick.BChan
import qualified Brick.Main as M
import Brick.Types
import Brick.Widgets.Border
import Brick.Widgets.Border.Style
import Brick.Widgets.Edit
import Brick.Widgets.List as L
import Data.List (head)
import Data.Text (splitOn)
import Data.Vector (Vector (..))
import qualified Data.Vector as V
import Graphics.Vty (defaultConfig)
import qualified Graphics.Vty as V
import Graphics.Vty.Platform.Unix (mkVty)
import Protolude hiding (decodeUtf8, head)
import System.Environment

import Control.Concurrent (forkIO)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C8
import Network.Socket
import Network.Socket.ByteString (recv, sendAll)

main :: IO ()
main = do
  chan <- newBChan 10

  userName <- getEnv "USER"

  let host = "127.0.0.1"
      port = "5000"
  addr <- head <$> getAddrInfo Nothing (Just host) (Just port)
  sock <- openSocket addr
  connect sock (addrAddress addr)

  sendAll sock (C8.pack userName)

  _ <-
    forkIO $
      let loop = do
            bs <- recv sock 4096
            if BS.null bs
              then pure ()
              else writeBChan chan (either (const $ panic "couldn't decode") identity (decodeUtf8 bs)) >> loop
       in loop

  let buildVty = mkVty defaultConfig
  initialVty <- buildVty
  void $ M.customMain initialVty buildVty (Just chan) theApp (AppState emptyHistory emptyEditor sock)

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
emptyHistory = L.list 1 V.empty 1

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
appEvent (VtyEvent e) = case e of
  V.EvKey (V.KEsc) [] -> M.halt
  V.EvKey (V.KEnter) [] -> do
    editorState <- gets sEditor
    sock <- gets sSock
    let msg = head $ getEditContents editorState
    liftIO $ sendAll sock (encodeUtf8 msg)
    modify (\s -> s{sHistory = L.list 1 (V.snoc (listElements $ sHistory s) (Own msg)) 1, sEditor = emptyEditor})
  _ -> do
    editorState <- gets sEditor
    newEditorState <- nestEventM' editorState $ handleEditorEvent (VtyEvent e)
    modify (\s -> s{sEditor = newEditorState})
appEvent (AppEvent msg) = do
  let [user,message] = splitOn "|" msg
  modify (\s -> s{sHistory = L.list 1 (V.snoc (listElements $ sHistory s) (Other user message)) 1})
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
  , sSock :: Socket
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
