{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module SpaceInvader.UI where

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
import qualified Data.List.NonEmpty as NE
import Data.Sequence.NonEmpty (index, singleton)
import Graphics.Vty
import qualified Graphics.Vty as V
import Graphics.Vty.Platform.Unix (mkVty)
import Lens.Micro ((^.))
import Lens.Micro.Mtl
import Lens.Micro.TH
import Linear
import SpaceInvader.Core
import System.Random

data AppState = AppState
  { _ship :: Coord
  , _projectiles :: [Coord]
  , _cover :: [Coord]
  , _enemies :: [Enemy]
  }
  deriving (Show)

makeLenses 'AppState

debug :: Bool
debug = True

data Tick = Tick | Tock

drawUI :: AppState -> [T.Widget ()]
drawUI s =
  if debug
    then addDebugInfo s game
    else game
 where
  game = [center $ drawGrid s]

addDebugInfo :: AppState -> [T.Widget ()] -> [T.Widget ()]
addDebugInfo (AppState s ps cover es) ws =
  vBox [str $ show s, str $ show ps, str $ show cover] : ws

drawGrid :: AppState -> T.Widget ()
drawGrid s =
  withBorderStyle BS.unicodeBold $
    B.borderWithLabel (str "Space Invader") $
      vBox rows
 where
  rows = [hBox $ cols r | r <- [0, 1 .. height]]
  cols r = [getCell r c | c <- [0, 1 .. width]]
  getCell x y
    | V2 x y == s ^. ship = withAttr shipAttr $ str "  "
    | V2 x y `elem` s ^. projectiles = withAttr projectileAttr $ str "''"
    | V2 x y `elem` s ^. cover = withAttr coverAttr $ str "  "
    | V2 x y `elem` join (s ^. enemies) = withAttr enemyAttr $ str "  "
    | otherwise = str "  "

left, right :: EventM () AppState ()
left = do
  s <- use ship
  unless
    (isOutside (moveLeft s))
    (ship %= moveLeft)
right = do
  s <- use ship
  unless
    (isOutside (moveRight s))
    (ship %= moveRight)

appEvent :: T.BrickEvent () Tick -> T.EventM () AppState ()
appEvent (T.VtyEvent e) = case e of
  V.EvKey V.KLeft [] -> left
  V.EvKey (V.KChar 'h') [] -> left
  V.EvKey V.KRight [] -> right
  V.EvKey (V.KChar 'l') [] -> right
  V.EvKey V.KEsc [] -> M.halt
  V.EvKey (V.KChar 'q') [] -> M.halt
  V.EvKey (V.KChar ' ') [] -> do
    s <- use ship
    projectiles %= shoot s
  _ -> return ()
appEvent (T.AppEvent Tick) = projectiles %= updateProjectiles
appEvent (T.AppEvent Tock) = enemies %= updateEnemies
appEvent _ = return ()

shipAttr :: A.AttrName
shipAttr = attrName "shipAttr"

projectileAttr :: A.AttrName
projectileAttr = attrName "projectileAttr"

coverAttr :: A.AttrName
coverAttr = attrName "coverAttr"

enemyAttr :: A.AttrName
enemyAttr = attrName "enemyAttr"

theMap :: A.AttrMap
theMap =
  A.attrMap
    V.defAttr
    [ (shipAttr, V.red `on` V.red)
    , (projectileAttr, V.red `on` V.black)
    , (coverAttr, V.green `on` V.green)
    , (enemyAttr, V.white `on` V.white)
    ]

theApp :: M.App AppState Tick ()
theApp =
  M.App
    { M.appDraw = drawUI
    , M.appChooseCursor = M.showFirstCursor
    , M.appHandleEvent = appEvent
    , M.appStartEvent = return ()
    , M.appAttrMap = const theMap
    }

initState :: AppState
initState =
  AppState
    { _ship = V2 30 15
    , _projectiles = []
    , _cover = initCover
    , _enemies = initEnemies
    }

main :: IO ()
main = do
  chan <- newBChan 10
  _ <- forkIO $ forever (writeBChan chan Tick >> threadDelay 140000)
  _ <- forkIO $ forever (writeBChan chan Tock >> threadDelay 1000000)
  let buildVty = mkVty defaultConfig
  initialVty <- buildVty
  void $ customMain initialVty buildVty (Just chan) theApp initState
