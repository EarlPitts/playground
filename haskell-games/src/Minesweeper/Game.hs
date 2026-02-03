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
import Data.List
import Data.List.Split (divvy)
import Data.Maybe
import qualified Graphics.Vty as V
import Lens.Micro.Mtl
import Lens.Micro.TH
import System.Random
import Debug.Trace (traceShowId)
import qualified Data.Array as A

data Pos = Pos {x :: Int, y :: Int}

data AppState = AppState
  { _board :: Board,
    _ended :: Bool,
    _pos :: Pos
  }

data Tile = Tile
  { covered :: Bool,
    mine :: Bool
  }
  deriving (Show, Eq)

newtype Board = Board
  { tiles :: [[Tile]]
  }
  deriving (Eq, Show)

makeLenses 'AppState

drawUI :: AppState -> [T.Widget ()]
drawUI s = [center $ renderTable $ drawBoard s._board s._pos]

drawBoard :: Board -> Pos -> Table ()
drawBoard b@(Board ts) pos = undefined -- table $ (fmap . fmap) f (zip [0..] (fmap (zip [0..]) ts))
  where
    f = undefined
    -- f (Tile True _) = str "    "
    -- f (Tile False True) = str "  X  "
    -- f (Tile False False) = str $ "  " <> (show $ adjacentMines b pos) <> "  "

adjacentMines :: Board -> Pos -> Int
adjacentMines (Board ts) (Pos x y) = length mines
  where
    up = (ts !? (x - 1)) >>= (!? y)
    down = (ts !? (x + 1)) >>= (!? y)
    left = (ts !? x) >>= (!? (y - 1))
    right = (ts !? x) >>= (!? (y + 1))
    mines = filter mine (catMaybes [up, down, left, right])

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

initBoard :: StdGen -> Board
initBoard gen = Board $ divvy 10 5 $ take 50 $ Tile False <$> mine
  where
    mine = uniforms gen :: [Bool]

initialState :: StdGen -> AppState
initialState gen = AppState (initBoard gen) False (Pos 4 2)

main :: IO ()
main = do
  gen <- initStdGen
  void $ M.defaultMain theApp (initialState gen)
