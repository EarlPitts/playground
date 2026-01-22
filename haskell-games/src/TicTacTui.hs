{-# LANGUAGE OverloadedStrings #-}

module TicTacTui where

import Brick
import qualified Brick.AttrMap as A
import qualified Brick.Main as M
import qualified Brick.Types as T
import Brick.Widgets.Center (center)
import qualified Brick.Widgets.List as L
import Brick.Widgets.Table
import Control.Monad
import Data.List
import Data.List.Split (divvy)
import Data.Text (pack)
import qualified Graphics.Vty as V

data Tile = X | O | Empty deriving (Eq)

data Board = Board [(Int, Tile)] deriving (Eq, Show)

data Pos = Pos {row :: Int, col :: Int} deriving (Show, Eq)

data AppState = AppState
  { board :: Board,
    pos :: Pos
  }
  deriving (Show, Eq)

instance Show Tile where
  show X = "X"
  show O = "O"
  show Empty = " "

drawUI :: AppState -> [Widget ()]
drawUI s = [center $ renderTable $ renderGrid s.board s.pos]

-- updateIdx :: Int -> a -> [a] -> [a]
-- updateIdx n a as = take (n) as <> [a] <> drop (n + 1) as

modifyIdx :: Int -> (a -> a) -> [a] -> [a]
modifyIdx n f as = take (n) as <> [f (as !! n)] <> drop (n + 1) as

renderGrid :: Board -> Pos -> Table ()
renderGrid (Board ts) (Pos r c) =
  table $
    divvy 3 3 $
      highlight $
        fmap
          ((padLeftRight 1) . txt . pack . show . snd)
          ts
  where
    -- ts' = updateIdx (3 * r + c) ((3 * r + c), X) ts
    highlight = modifyIdx (3 * r + c) undefined

initBoard :: Board
initBoard = Board $ zip [1 .. 9] (repeat Empty)

initialState = AppState initBoard (Pos 0 0)

theMap :: A.AttrMap
theMap =
  A.attrMap
    V.defAttr
    [ (L.listAttr, V.white `on` V.blue)
    -- , (selectedCellAttr,      V.blue `on` V.white)
    ]

appEvent :: T.BrickEvent () e -> T.EventM () AppState ()
appEvent (T.VtyEvent e) = case e of
  -- V.EvKey (V.KChar '+') [] -> do
  --     els <- use (tabularList.L.listElementsL)
  --     let el = Row (show pos) (show $ pos * 3) (show $ pos * 9)
  --         pos = Vec.length els
  --     tabularList %= L.listInsert pos el
  --
  -- V.EvKey (V.KChar '-') [] -> do
  --     sel <- use (tabularList.L.listSelectedL)
  --     case sel of
  --         Nothing -> return ()
  --         Just i -> tabularList %= L.listRemove i

  V.EvKey V.KLeft [] -> do
    (AppState b (Pos r c)) <- get
    put (AppState b (Pos r (c - 1)))
  V.EvKey V.KRight [] -> do
    (AppState b (Pos r c)) <- get
    put (AppState b (Pos r (c + 1)))
  V.EvKey V.KUp [] -> do
    (AppState b (Pos r c)) <- get
    put (AppState b (Pos (r - 1) c))
  V.EvKey V.KDown [] -> do
    (AppState b (Pos r c)) <- get
    put (AppState b (Pos (r + 1) c))
  V.EvKey (V.KChar ' ') [] -> do
    (AppState (Board ts) p@(Pos r c)) <- get
    put (AppState (Board (modifyIdx (3 * r + c) (const (3 * r + c, X)) ts)) p)
  V.EvKey V.KEsc [] -> M.halt
  _ -> return ()
-- ev -> T.zoom tabularList $ L.handleListEvent ev
appEvent _ = return ()

theApp :: M.App AppState e ()
theApp =
  M.App
    { M.appDraw = drawUI,
      M.appChooseCursor = M.showFirstCursor,
      M.appHandleEvent = appEvent,
      M.appStartEvent = return (),
      M.appAttrMap = const theMap
    }

main :: IO ()
main = void $ M.defaultMain theApp initialState
