{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Snake.Main where

import Brick
import qualified Brick.AttrMap as A
import qualified Brick.Main as M
import qualified Brick.Types as T
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Border.Style as BS
import Brick.Widgets.Center (center, hCenter)
import Brick.Widgets.Table
import Control.Monad
import qualified Data.Array as A
import Data.Array.IArray (assocs)
import Data.Bifunctor
import qualified Data.List.NonEmpty as NE
import Data.List.Split (divvy)
import Data.Sequence (Seq ((:<|), (:|>)), index, singleton)
import qualified Graphics.Vty as V
import Lens.Micro ((^.))
import Lens.Micro.Mtl
import Lens.Micro.TH
import Linear
import Snake.Snake
import System.Random

data AppState = AppState
  { _snake :: Snake,
    _food :: Food,
    _eaten :: [Food]
  }

makeLenses 'AppState

drawUI :: AppState -> [T.Widget ()]
drawUI s = [center (drawGrid s)]

height, width :: Int
height = 10
width = 10

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

snakeHead (h :<| _) = h

up, down, left, right :: EventM () AppState ()
up = proceed U
down = proceed D
left = proceed L
right = proceed R

proceed dir = do
  currFood <- use food
  s <- use snake
  if ate s currFood
    then do
      food .= V2 1 1
      snake .= ((move dir (singleton (snakeHead s))) <> s)
    else snake %= (move dir)

appEvent :: T.BrickEvent () e -> T.EventM () AppState ()
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

theApp :: M.App AppState e ()
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
      _eaten = []
    }

main :: IO ()
main = do
  void $ M.defaultMain theApp initState
