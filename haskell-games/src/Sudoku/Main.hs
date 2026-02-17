{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Sudoku.Main where

import Brick
import qualified Brick.AttrMap as A
import Brick.BChan
import qualified Brick.Main as M
import qualified Brick.Types as T
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Border.Style as BS
import Brick.Widgets.Center (center, hCenter)
import Brick.Widgets.Table (renderTable, table)
import Control.Concurrent
import Control.Monad
import Control.Monad.IO.Class (liftIO)
import Data.List (intercalate, intersperse)
import qualified Data.List.NonEmpty as NE
import Data.Sequence.NonEmpty (index, singleton)
import Graphics.Vty
import qualified Graphics.Vty as V
import Graphics.Vty.Platform.Unix (mkVty)
import Lens.Micro ((^.))
import Lens.Micro.Mtl
import Lens.Micro.TH
import Snake.Snake
import System.Random
import Sudoku.Sudoku

data AppState = AppState {} deriving (Show)

makeLenses 'AppState

drawUI :: AppState -> [T.Widget ()]
drawUI s = [center $ drawGrid]

drawGrid :: T.Widget ()
drawGrid =
  renderTable $
    table rows
  where
    rows = [cols r | r <- [0, 1 .. 8]]
    cols r = [getCell r c | c <- [0, 1 .. 8]]
    getCell x y = str "   "

up, down, left, right :: EventM () AppState ()
up = undefined
down = undefined
left = undefined
right = undefined

appEvent :: T.BrickEvent () () -> T.EventM () AppState ()
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

theMap :: A.AttrMap
theMap =
  A.attrMap
    V.defAttr
    []

theApp :: M.App AppState () ()
theApp =
  M.App
    { M.appDraw = drawUI,
      M.appChooseCursor = M.showFirstCursor,
      M.appHandleEvent = appEvent,
      M.appStartEvent = return (),
      M.appAttrMap = const theMap
    }

initState :: AppState
initState = AppState {}

main :: IO ()
main =
  void $ M.defaultMain theApp initState
