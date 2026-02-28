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
  }
  deriving (Show)

makeLenses 'AppState

debug :: Bool
debug = False

data Tick = Tick

drawUI :: AppState -> [T.Widget ()]
drawUI s = [center $ drawGrid s]

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
    | otherwise = str "  "

left, right :: EventM () AppState ()
left = ship %= moveLeft
right = ship %= moveRight

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
appEvent _ = return ()

shipAttr :: A.AttrName
shipAttr = attrName "shipAttr"

projectileAttr :: A.AttrName
projectileAttr = attrName "projectileAttr"

theMap :: A.AttrMap
theMap =
  A.attrMap
    V.defAttr
    [ (shipAttr, V.black `on` V.red)
    , (projectileAttr, V.red `on` V.black)
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
    { _ship = V2 29 15
    , _projectiles = []
    }

main :: IO ()
main = do
  chan <- newBChan 10
  _ <- forkIO $ forever (writeBChan chan Tick >> threadDelay 140000)
  let buildVty = mkVty defaultConfig
  initialVty <- buildVty
  void $ customMain initialVty buildVty (Just chan) theApp initState
