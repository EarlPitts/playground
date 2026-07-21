module Client (main) where

import Brick
import qualified Brick.AttrMap as A
import Brick.BChan
import Brick.Focus
import qualified Brick.Main as M
import Brick.Widgets.Border
import Brick.Widgets.Border.Style
import Brick.Widgets.Edit
import Brick.Widgets.List as L
import Brick.Widgets.List.Extended as L
import Codec.Serialise
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C8
import Data.List (head)
import qualified Data.List.NonEmpty as NE
import qualified Graphics.Vty as V
import Graphics.Vty.Platform.Unix (mkVty)
import Network.Socket
import Network.Socket.ByteString (recv, sendAll)
import Protolude hiding (decodeUtf8, head)
import System.Environment

import Core

withSocket :: AddrInfo -> (Socket -> IO a) -> IO a
withSocket addr = bracket (new addr) close

new :: AddrInfo -> IO Socket
new addr = do
  sock <- openSocket addr
  connect sock (addrAddress addr)
  pure sock

main :: IO ()
main = do
  chan <- newBChan 10
  userName <- getEnv "USER"

  addr <-
    NE.head
      <$> getAddrInfo
        Nothing
        (Just (toS $ cHost defaultConfig))
        (Just (toS $ cPort defaultConfig))

  withSocket addr $ \sock -> do
    sendAll sock (C8.pack userName)

    void $ forkIO $ forever $ receiveMessage chan sock

    let buildVty = mkVty V.defaultConfig
    initialVty <- buildVty
    void $
      M.customMain
        initialVty
        buildVty
        (Just chan)
        theApp
        (initState sock)

receiveMessage :: BChan Message -> Socket -> IO ()
receiveMessage chan sock = do
  bs <- recv sock 4096
  if BS.null bs
    then pure ()
    else
      writeBChan
        chan
        (either (const $ panic "couldn't decode") identity (fromServer (deserialise (BS.fromStrict bs))))

data Name
  = Editor
  | History
  deriving (Show, Eq, Ord)

emptyEditor = editor Editor (Just 1) mempty
emptyHistory = L.list History mempty 1

initState :: Socket -> AppState
initState sock =
  AppState
    { sHistory = emptyHistory
    , sEditor = emptyEditor
    , sSock = sock
    , sFocus = focusRing [Editor, History]
    }

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

appEvent :: BrickEvent Name Message -> EventM Name AppState ()
appEvent (VtyEvent e) = case e of
  V.EvKey (V.KEsc) [] -> M.halt
  V.EvKey (V.KEnter) [] -> do
    editorState <- gets sEditor
    sock <- gets sSock
    let msg = head $ getEditContents editorState
    liftIO $ sendAll sock (encodeUtf8 msg)
    modify (\s -> s{sHistory = listMoveToEnd $ L.listAppend (Own msg) (sHistory s), sEditor = emptyEditor})
  _ -> do
    AppState{..} <- get
    newEditorState <- nestEventM' sEditor $ handleEditorEvent (VtyEvent e)
    newHistoryState <- nestEventM' sHistory $ handleListEvent e
    modify (\s -> s{sEditor = newEditorState, sHistory = newHistoryState})
appEvent (AppEvent msg) = do
  modify (\s -> s{sHistory = listMoveToEnd $ L.listAppend msg (sHistory s)})
appEvent _ = pure ()

data Message
  = Other
      { mUsername :: Text
      , mText :: Text
      }
  | Own {mText :: Text}
  deriving (Show, Eq)

fromServer :: ServerMessage -> Either Text Message
fromServer (ServerMessage user msg) = do
  userText <- first show (decodeUtf8 user)
  msgText <- first show (decodeUtf8 msg)
  pure (Other userText msgText)

data AppState = AppState
  { sHistory :: List Name Message
  , sEditor :: Editor Text Name
  , sSock :: Socket
  , sFocus :: FocusRing Name
  }

theApp :: M.App AppState Message Name
theApp =
  M.App
    { M.appDraw = drawUI
    , M.appChooseCursor = M.showFirstCursor
    , M.appHandleEvent = appEvent
    , M.appStartEvent = pure ()
    , M.appAttrMap = const $ A.attrMap V.defAttr []
    }
