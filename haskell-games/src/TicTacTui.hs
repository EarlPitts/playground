{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module TicTacTui where

import Brick
import qualified Brick.AttrMap as A
import qualified Brick.Main as M
import qualified Brick.Types as T
import qualified Brick.Widgets.Border.Style as S
import Brick.Widgets.Center (center)
import qualified Brick.Widgets.List as L
import Brick.Widgets.Table
import Control.Monad
import Data.Foldable
import Data.List
import Data.List (intercalate, isInfixOf)
import Data.List.Split (chunksOf, divvy)
import Data.Text (pack)
import Debug.Trace
import qualified Graphics.Vty as V
import Lens.Micro ((^.))
import Lens.Micro.Mtl
import Lens.Micro.TH
import System.Console.Haskeline
import System.Random
import Text.Read (readMaybe)

data Tile = X | O | Empty deriving (Eq)

data Board = Board {tiles :: [Tile]} deriving (Eq)

data Pos = Pos {row :: Int, col :: Int} deriving (Show, Eq)

data Player = Computer | Human deriving (Show, Eq)

data AppState = AppState
  { _board :: Board,
    _pos :: Pos,
    _gen :: StdGen,
    _winner :: Maybe Player
  }
  deriving (Eq)

makeLenses ''AppState

instance Show Tile where
  show X = "X"
  show O = "O"
  show Empty = " "

drawUI :: AppState -> [Widget ()]
drawUI s = case s._winner of
  Just Human -> [center $ txt "You won!"]
  Just Computer -> [center $ txt "You lost :("]
  Nothing -> [center $ renderTable $ renderGrid s._board s._pos]

modifyIdx :: Int -> (a -> a) -> [a] -> [a]
modifyIdx n f as = take (n) as <> [f (as !! n)] <> drop (n + 1) as

renderGrid :: Board -> Pos -> Table ()
renderGrid (Board ts) (Pos r c) =
  table $
    divvy 3 3 $
      highlight $
        ((padLeftRight 1) . txt . pack . show) <$> ts
  where
    highlight = modifyIdx (3 * r + c) (withAttr highlighted)

initBoard :: Board
initBoard = Board $ (replicate 9 Empty)

initialState = AppState initBoard (Pos 1 1) (mkStdGen 1) Nothing -- TODO

highlighted :: A.AttrName
highlighted = attrName "highlighted"

theMap :: A.AttrMap
theMap =
  A.attrMap
    V.defAttr
    [(highlighted, bg V.yellow)]

appEvent :: T.BrickEvent () e -> T.EventM () AppState ()
appEvent (T.VtyEvent e) = case e of
  V.EvKey V.KLeft [] ->
    pos %= (\(Pos r c) -> Pos r (c - 1))
  V.EvKey V.KRight [] ->
    pos %= (\(Pos r c) -> Pos r (c + 1))
  V.EvKey V.KUp [] ->
    pos %= (\(Pos r c) -> Pos (r - 1) c)
  V.EvKey V.KDown [] ->
    pos %= (\(Pos r c) -> Pos (r + 1) c)
  V.EvKey (V.KChar ' ') [] -> select
  V.EvKey V.KEsc [] -> M.halt
  V.EvKey (V.KChar 'q') [] -> M.halt
  _ -> return ()
appEvent _ = return ()

currentTile :: AppState -> Tile
currentTile (AppState (Board ts) (Pos r c) _ _) = ts !! (r * 3 + c)

select :: T.EventM () AppState ()
select = do
  s@(AppState (Board ts) p@(Pos r c) gen w) <- get
  case currentTile s of
    Empty -> do
      let newState = (AppState (Board (modifyIdx (3 * r + c) (const X) ts)) p gen w)
      if checkWin newState._board X
        then winner %= (const $ Just Human)
        else do
          let (newGen, newBoard) = computerStep newState._board gen
          let newerState = AppState newBoard p newGen w
          if checkWin newerState._board O
            then winner %= (const $ Just Computer)
            else put newerState
    _ -> return ()

theApp :: M.App AppState e ()
theApp =
  M.App
    { M.appDraw = drawUI,
      M.appChooseCursor = M.showFirstCursor,
      M.appHandleEvent = appEvent,
      M.appStartEvent = return (),
      M.appAttrMap = const theMap
    }

data InputError = EOF | InvalidMove | Unexpected deriving (Show, Eq)

getMove :: [Int] -> IO (Either InputError Int)
getMove available = do
  input <-
    runInputT
      defaultSettings
      (getInputLine "Next move: ")
  case input of
    Nothing -> pure $ Left EOF
    Just n -> case readMaybe n :: Maybe Int of
      Just n -> if n `elem` available then pure $ Right n else pure $ Left InvalidMove
      Nothing -> pure (Left Unexpected)

updateState :: Tile -> Int -> Board -> Board
updateState tile n (Board ts) =
  Board $
    fmap snd $
      fmap
        (\(n', t') -> if n == n' then (n, tile) else (n', t'))
        (zip [0 ..] ts)

getAvailable :: Board -> [Int]
getAvailable (Board ts) =
  fmap fst $ filter (\(_, tile) -> tile == Empty) (zip [0 ..] ts)

computerStep :: Board -> StdGen -> (StdGen, Board)
computerStep b gen =
  let available = getAvailable b
      (n, newGen) = randomR (0, pred $ length available) gen
      choice = available !! n
   in (newGen, updateState O choice b)

checkWin :: Board -> Tile -> Bool
checkWin b t =
  any (`isInfixOf` nums) (horizontal <> vertical <> across)
  where
    horizontal = [[0, 1, 2], [3, 4, 5], [6, 7, 8]]
    vertical = [[0, 3, 6], [1, 4, 7], [2, 5, 8]]
    across = [[0, 4, 8], [2, 4, 6]]
    nums = getTiles b t

getTiles :: Board -> Tile -> [Int]
getTiles (Board b) t = fmap fst (filter (\(_, tile) -> tile == t) (zip [0 ..] b))

main :: IO ()
main = void $ M.defaultMain theApp initialState
