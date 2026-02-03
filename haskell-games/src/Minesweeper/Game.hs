{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Minesweeper.Game where

import Brick
import qualified Brick.AttrMap as A
import qualified Brick.Main as M
import qualified Brick.Types as T
import Brick.Widgets.Center (center)
import Brick.Widgets.Table
import Control.Monad
import Control.Monad.Identity
import qualified Data.Array as A
import Data.Array.IArray (elems)
import Data.List
import Data.List.Split (divvy)
import Data.Maybe
import Debug.Trace (traceShowId)
import qualified Graphics.Vty as V
import Lens.Micro.Mtl
import Lens.Micro.TH
import Minesweeper.Grid
import System.Random

data AppState = AppState
  { _board :: Grid,
    _ended :: Bool,
    _coord :: Coord
  }

makeLenses 'AppState

drawUI :: AppState -> [T.Widget ()]
drawUI s = [center $ renderTable $ drawGrid s._board s._coord]

drawGrid :: Grid -> Coord -> Table ()
drawGrid g selected = table $ divvy 5 5 $ f <$> (elems g)
  where
    f (Tile True _) = str "    "
    f (Tile False True) = str "  X  "
    f (Tile False False) = str $ "  " <> (show $ mineCount g selected) <> "  "

appEvent = undefined

theMap :: A.AttrMap
theMap = A.attrMap V.defAttr []

theApp :: M.App AppState e ()
theApp =
  M.App
    { M.appDraw = drawUI,
      M.appChooseCursor = M.showFirstCursor,
      M.appHandleEvent = appEvent,
      M.appStartEvent = return (),
      M.appAttrMap = const theMap
    }

initBoard :: StdGen -> Grid
initBoard gen = mkGrid (10, 5) $ take 50 $ Tile False <$> mine
  where
    mine = uniforms gen :: [Bool]

initialState :: StdGen -> AppState
initialState gen = AppState (initBoard gen) False (4, 2)

main :: IO ()
main = do
  gen <- initStdGen
  void $ M.defaultMain theApp (initialState gen)
