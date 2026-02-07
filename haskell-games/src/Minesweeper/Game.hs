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
import Data.Array.IArray (assocs)
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
drawUI s =
  if s._ended
    then [str "ended", center $ renderTable $ drawGrid s._board s._coord]
    else [center $ renderTable $ drawGrid s._board s._coord]

drawGrid :: Grid -> Coord -> Table ()
drawGrid g selected = table $ divvy 5 5 $ highlight $ f <$> (assocs g)
  where
    f (coord, (Tile True _)) = str "    "
    f (coord, (Tile False True)) = str "  X  "
    f (coord, (Tile False False)) = str $ "  " <> (show $ mineCount g coord) <> "  "
    highlight = modifyIdx (5 * (pred $ fst selected) + (pred $ snd selected)) (withAttr highlighted)

modifyIdx :: Int -> (a -> a) -> [a] -> [a]
modifyIdx n f as = take (n) as <> [f (as !! n)] <> drop (n + 1) as

up, down, left, right :: EventM () AppState ()
up = do
  (r, c) <- use coord
  when (r > 1) (coord .= ((r - 1), c))
down = do
  (r, c) <- use coord
  when (r < 10) (coord .= ((r + 1), c))
left = do
  (r, c) <- use coord
  when (c > 1) (coord .= (r, (c - 1)))
right = do
  (r, c) <- use coord
  when (c < 5) (coord .= (r, (c + 1)))

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
  V.EvKey (V.KChar ' ') [] -> select
  V.EvKey V.KEsc [] -> M.halt
  V.EvKey (V.KChar 'q') [] -> M.halt
  _ -> return ()
appEvent _ = return ()

select :: T.EventM () AppState ()
select = do
  g <- use board
  coord <- use coord
  board .= reveal g coord
  g' <- use board
  if mineUncovered g' then ended .= True else pure ()

highlighted :: A.AttrName
highlighted = attrName "highlighted"

theMap :: A.AttrMap
theMap =
  A.attrMap
    V.defAttr
    [(highlighted, V.black `on` V.yellow)]

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
initBoard gen = mkGrid (10, 5) $ take 50 $ Tile True <$> mine
  where
    (mine, _) = uniformShuffleList (replicate 6 True <> replicate 44 False) gen

-- mine = repeat False
-- mine = uniforms gen :: [Bool]

initialState :: StdGen -> AppState
initialState gen = AppState (initBoard gen) False (4, 2)

main :: IO ()
main = do
  gen <- initStdGen
  void $ M.defaultMain theApp (initialState gen)
