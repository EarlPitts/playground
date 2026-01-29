{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Minesweeper where

import Brick
import qualified Brick.AttrMap as A
import qualified Brick.Main as M
import qualified Brick.Types as T
import Brick.Widgets.Center (center)
import Brick.Widgets.Table
import Control.Monad
import qualified Graphics.Vty as V
import Lens.Micro.Mtl
import Lens.Micro.TH

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
drawUI s = [center $ renderTable $ drawBoard s._board]

drawBoard :: Board -> Table ()
drawBoard (Board ts) = table $ (fmap . fmap) f ts
  where
    f (Tile True m) = str "    "
    f (Tile False True) = str "X"
    f (Tile False False) = str "1"

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

initialState :: AppState
initialState = AppState (Board (replicate 10 (replicate 5 (Tile True False)))) False (Pos 4 2)

main :: IO ()
main = void $ M.defaultMain theApp initialState
