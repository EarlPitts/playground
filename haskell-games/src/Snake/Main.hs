{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Snake.Main where

import Brick
import qualified Brick.AttrMap as A
import Brick.BChan
import qualified Brick.Main as M
import qualified Brick.Types as T
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Border.Style as BS
import Brick.Widgets.Center (center)
import Control.Concurrent
import Control.Monad
import Control.Monad.IO.Class (liftIO)
import qualified Data.List.NonEmpty as NE
import Data.Sequence.NonEmpty (singleton, index)
import Graphics.Vty
import qualified Graphics.Vty as V
import Graphics.Vty.Platform.Unix (mkVty)
import Lens.Micro ((^.))
import Lens.Micro.Mtl
import Lens.Micro.TH
import Linear
import Snake.Snake
import System.Random

data AppState = AppState
  { _snake :: Snake,
    _food :: Food,
    _ended :: Bool,
    _dir :: Dir
  }
  deriving (Show)

makeLenses 'AppState

debug :: Bool
debug = True

data Tick = Tick

drawUI :: AppState -> [T.Widget ()]
drawUI s =
  if debug
    then addDebugInfo s game
    else game
  where
    game =
      if s ^. ended
        then [center $ str "Game Over"]
        else [center (drawGrid s)]

addDebugInfo :: AppState -> [T.Widget ()] -> [T.Widget ()]
addDebugInfo (AppState s f e cnt) ws =
  (vBox $ [str $ show s, str $ show f, str $ show e, str $ show cnt]) : ws

drawGrid :: AppState -> T.Widget ()
drawGrid s =
  withBorderStyle BS.unicodeBold $
    B.borderWithLabel (str "Snake") $
      vBox rows
  where
    rows = [hBox $ cols r | r <- [0, 1 .. height]]
    cols r = [getCell r c | c <- [0, 1 .. width]]
    getCell x y =
      if (V2 x y) == (index (s ^. snake) 0)
        then (withAttr snakeHeadAttr $ str "..")
        else
          ( if (V2 x y) `elem` s ^. snake
              then (withAttr snakeAttr $ str "  ")
              else (if (V2 x y) == s ^. food then (withAttr foodAttr $ str "` ") else (str "  "))
          )

up, down, left, right :: EventM () AppState ()
up = dir .= U
down = dir .= D
left = dir .= L
right = dir .= R

proceed :: Dir -> EventM () AppState ()
proceed dir = do
  currFood <- use food
  s <- use snake
  if ate s currFood
    then do
      let newSnake = ((move dir (singleton (snakeHead s))) <> s)
      snake .= newSnake
      let empty = emptyCells newSnake
      foodIdx <- liftIO $ randomRIO (0, length empty)
      food .= empty !! foodIdx
    else snake %= (move dir)
  s <- use snake
  if collision s
    then ended .= True
    else pure ()

appEvent :: T.BrickEvent () Tick -> T.EventM () AppState ()
appEvent (T.VtyEvent e) = case e of
  V.EvKey V.KLeft [] -> left
  V.EvKey (V.KChar 'h') [] -> left
  V.EvKey V.KRight [] -> right
  V.EvKey (V.KChar 'l') [] -> right
  V.EvKey V.KUp [] -> up
  V.EvKey (V.KChar 'k') [] -> up
  V.EvKey V.KDown [] -> down
  V.EvKey (V.KChar 'j') [] -> down
  V.EvKey V.KEsc [] -> M.halt
  V.EvKey (V.KChar 'q') [] -> M.halt
  _ -> return ()
appEvent (T.AppEvent Tick) = use dir >>= proceed
appEvent _ = return ()

snakeAttr :: A.AttrName
snakeAttr = attrName "snakeAttr"

snakeHeadAttr :: A.AttrName
snakeHeadAttr = attrName "snakeHeadAttr"

foodAttr :: A.AttrName
foodAttr = attrName "foodAttr"

theMap :: A.AttrMap
theMap =
  A.attrMap
    V.defAttr
    [ (snakeAttr, V.green `on` V.green),
      (snakeHeadAttr, V.black `on` V.green),
      (foodAttr, V.black `on` V.red)
    ]

theApp :: M.App AppState Tick ()
theApp =
  M.App
    { M.appDraw = drawUI,
      M.appChooseCursor = M.showFirstCursor,
      M.appHandleEvent = appEvent,
      M.appStartEvent = return (),
      M.appAttrMap = const theMap
    }

initState :: AppState
initState =
  AppState
    { _snake = snakeFromList (NE.fromList [(3, 1), (2, 1), (1, 1)]),
      _food = V2 8 8,
      _ended = False,
      _dir = D
    }

main :: IO ()
main = do
  chan <- newBChan 10
  _ <- forkIO $ forever $ (writeBChan chan Tick >> threadDelay 100000)
  let buildVty = mkVty defaultConfig
  initialVty <- buildVty
  void $ customMain initialVty buildVty (Just chan) theApp initState
