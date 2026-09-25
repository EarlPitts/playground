{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Pacman.Pacman where

import Brick
import qualified Brick.AttrMap as A
import Brick.BChan
import qualified Brick.Main as M
import qualified Brick.Types as T
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Border.Style as BS
import Brick.Widgets.Center (center, hCenter)
import Control.Concurrent
import Control.Monad
import Control.Monad.IO.Class (liftIO)
import Graphics.Vty hiding (update)
import qualified Graphics.Vty as V
import Graphics.Vty.Platform.Unix (mkVty)
import Lens.Micro ((^.))
import Lens.Micro.Mtl hiding (view)
import Lens.Micro.TH
import Linear

data Model = Model
  { mPacman :: Pacman
  -- , mGhosts :: [Ghost]
  }
  deriving (Show, Eq)

type Coord = V2 Int

data Dir = L | R | D | U deriving (Show, Eq)

data Ghost = Ghost {gCoord :: Coord} deriving (Show, Eq)
data Pacman = Pacman
  { pCoord :: Coord
  , pDir :: Dir
  }
  deriving (Show, Eq)

makeLenses 'Model
makeLenses 'Pacman

view :: Model -> [T.Widget ()]
view m = [center $ padRight (Pad 2) $ viewGrid m]

height = 10
width = 10

viewGrid :: Model -> T.Widget ()
viewGrid m =
  withBorderStyle BS.unicodeBold $
    B.borderWithLabel (str "PacMan") $
      vBox rows
 where
  rows = [hBox $ cols r | r <- [0, 1 .. height]]
  cols r = [getCell r c | c <- [0, 1 .. width]]
  getCell x y
    | V2 x y == pCoord (mPacman m) = withAttr snakeHeadAttr $ str "O"
    | otherwise = str "  "

data Tick = Tick

up, down, left, right :: EventM () Model ()
up = modify (\m -> m{mPacman = (mPacman m){pDir = U}})
down = modify (\m -> m{mPacman = (mPacman m){pDir = D}})
left = modify (\m -> m{mPacman = (mPacman m){pDir = L}})
right = modify (\m -> m{mPacman = (mPacman m){pDir = R}})

update :: T.BrickEvent () Tick -> T.EventM () Model ()
update (T.VtyEvent e) = case e of
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
update (T.AppEvent Tick) = undefined
update _ = return ()

-- step :: Model -> Model
-- step Model{..} = Model (stepPacman mPacman)

theMap :: A.AttrMap
theMap =
  A.attrMap
    V.defAttr
    [ (pacmanAttr, V.green `on` V.green)
    ]

pacmanAttr :: A.AttrName
pacmanAttr = attrName "pacmanAttr"

snakeHeadAttr :: A.AttrName
snakeHeadAttr = attrName "snakeHeadAttr"

foodAttr :: A.AttrName
foodAttr = attrName "foodAttr"

theApp :: M.App Model Tick ()
theApp =
  M.App
    { M.appDraw = view
    , M.appChooseCursor = M.showFirstCursor
    , M.appHandleEvent = update
    , M.appStartEvent = return ()
    , M.appAttrMap = const theMap
    }

initState :: Model
initState =
  Model
    { mPacman = Pacman (V2 25 25) L
    }

main :: IO ()
main = do
  chan <- newBChan 10
  _ <- forkIO $ forever (writeBChan chan Tick >> threadDelay 140000)
  let buildVty = mkVty defaultConfig
  initialVty <- buildVty
  void $ customMain initialVty buildVty (Just chan) theApp initState
