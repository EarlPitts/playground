module Client (main) where

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

import Brick.Focus
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
  void $ M.customMain initialVty buildVty (Just chan) theApp (AppState emptyHistory emptyEditor sock (focusRing [Editor, History]))

data Name
  = Editor
  | History
  deriving (Show, Eq, Ord)

emptyEditor = editor Editor (Just 1) mempty
emptyHistory = L.list History V.empty 1

drawUI :: AppState -> [Widget Name]
drawUI AppState{..} = [vBox [history sHistory, textBox sEditor]]

history :: List Name Message -> Widget Name
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

textBox :: Editor Text Name -> Widget Name
textBox =
  borderWithLabel (txt "Reply")
    . renderEditor (\t -> txt $ head t) True

appEvent :: BrickEvent Name Text -> EventM Name AppState ()
appEvent (VtyEvent e) = case e of
  V.EvKey (V.KEsc) [] -> M.halt
  -- V.EvKey (V.KChar '\t') [] -> do
  --   curr <- gets (\s -> focusGetCurrent $ sFocus s)
  --   liftIO $ appendFile "sajtFile.txt" (show curr)
  --   modify (\s -> s{sFocus = focusNext $ sFocus s})
  V.EvKey (V.KEnter) [] -> do
    editorState <- gets sEditor
    sock <- gets sSock
    let msg = head $ getEditContents editorState
    liftIO $ sendAll sock (encodeUtf8 msg)
    modify (\s -> s{sHistory = listMoveToEnd $ listAppend (Own msg) (sHistory s), sEditor = emptyEditor})
  _ -> do
    AppState{..} <- get
    newEditorState <- nestEventM' sEditor $ handleEditorEvent (VtyEvent e)
    newHistoryState <- nestEventM' sHistory $ handleListEvent e
    modify (\s -> s{sEditor = newEditorState, sHistory = newHistoryState})
appEvent (AppEvent msg) = do
  let [user, message] = splitOn "|" msg
  modify (\s -> s{sHistory = listMoveToEnd $ listAppend (Other user message) (sHistory s)})
appEvent _ = pure ()

listAppend :: e -> List n e -> List n e
listAppend e l = listInsert len e l
 where
  len = length $ listElements l

data Message
  = Other
      { mUsername :: Text
      , mText :: Text
      }
  | Own {mText :: Text}
  deriving (Show, Eq)

data AppState = AppState
  { sHistory :: List Name Message
  , sEditor :: Editor Text Name
  , sSock :: Socket
  , sFocus :: FocusRing Name
  }

theApp :: M.App AppState Text Name
theApp =
  M.App
    { M.appDraw = drawUI
    , M.appChooseCursor = M.showFirstCursor
    , M.appHandleEvent = appEvent
    , M.appStartEvent = pure ()
    , M.appAttrMap = const $ A.attrMap V.defAttr []
    }
