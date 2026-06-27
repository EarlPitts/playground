{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Mandelbrot where

import Brick
import qualified Brick.AttrMap as A
import qualified Brick.Main as M
import Control.Monad (void)
import Data.Char
import Data.Complex
import Data.List
import Data.List.Split
import qualified Graphics.Vty as V
import System.Console.Terminal.Size (Window (..))
import qualified System.Console.Terminal.Size as Term

main :: IO ()
main = do
  Just (Window h w) <- Term.size
  void $ M.defaultMain (theApp w h) initState

data AppState = AppState
  { sZoom :: Double
  , sCenterX :: Double
  , sCenterY :: Double
  }
  deriving (Show)

theApp :: Int -> Int -> App AppState () ()
theApp w h =
  M.App
    { M.appDraw = drawUI w h
    , M.appChooseCursor = M.showFirstCursor
    , M.appHandleEvent = appEvent
    , M.appStartEvent = return ()
    , M.appAttrMap = const (A.attrMap V.defAttr [])
    }

initState :: AppState
initState =
  AppState
    { sZoom = 1
    , sCenterX = 0
    , sCenterY = 0
    }

drawUI :: Int -> Int -> AppState -> [Widget ()]
drawUI w h (AppState zoom centerX centerY) =
  [str $ Mandelbrot.render (mandelbrot zoom centerX centerY (w * 2) (h * 4)) w h]

appEvent :: BrickEvent () e -> EventM () AppState ()
appEvent (VtyEvent (V.EvKey key _)) = case key of
  V.KChar 'i' -> zoomIn
  V.KChar ' ' -> zoomIn
  V.KChar 'o' -> zoomOut
  V.KBS -> zoomOut
  V.KChar 'j' -> down
  V.KDown -> down
  V.KChar 'k' -> up
  V.KUp -> up
  V.KChar 'h' -> left
  V.KLeft -> left
  V.KChar 'l' -> right
  V.KRight -> right
  V.KChar 'q' -> M.halt
  _ -> return ()
appEvent _ = return ()

left, right, up, down, zoomIn, zoomOut :: EventM () AppState ()
left = modify (\s -> s{sCenterX = s.sCenterX - (0.5 / s.sZoom)})
down = modify (\s -> s{sCenterY = s.sCenterY + (0.5 / s.sZoom)})
up = modify (\s -> s{sCenterY = s.sCenterY - (0.5 / s.sZoom)})
right = modify (\s -> s{sCenterX = s.sCenterX + (0.5 / s.sZoom)})
zoomIn = modify (\s -> s{sZoom = s.sZoom * 1.5})
zoomOut = modify (\s -> s{sZoom = s.sZoom * 0.5})

type Cell = [[Bool]] -- 2x8
type Canvas = [[Bool]] -- arbitrary size

mandelbrot :: Double -> Double -> Double -> Int -> Int -> (Int, Int) -> Bool
mandelbrot zoom centerX centerY w h (x, y) = 2 > magnitude (values !! iters)
 where
  f c z = z * z + c
  nx = centerX + (fromIntegral x / fromIntegral w - 0.5) * 3.5 / zoom
  ny = centerY + (fromIntegral y / fromIntegral h - 0.5) * 2.0 / zoom
  iters = 80
  values = iterate (f (nx :+ ny)) 0

render :: ((Int, Int) -> Bool) -> Int -> Int -> String
render f width height =
  intercalate "\n" $ (fmap . fmap) renderCell $ discretize $ chunksOf (width * 2) (fmap f canvas)
 where
  canvas = [(x, y) | y <- [0 .. height * 4 - 1], x <- [0 .. width * 2 - 1]]

discretize :: Canvas -> [[Cell]]
discretize [] = []
discretize (r1 : r2 : r3 : r4 : rs) = f r1 r2 r3 r4 : discretize rs
 where
  f [] [] [] [] = []
  f (_1 : _4 : cs) (_2 : _5 : cs') (_3 : _6 : cs'') (_7 : _8 : cs''') = [[[_1, _4], [_2, _5], [_3, _6], [_7, _8]]] <> f cs cs' cs'' cs'''

renderCell :: Cell -> Char
renderCell = chr . (+ 0x2800) . fromIntegral . toOffset

toOffset :: Cell -> Int
toOffset [[_1, _4], [_2, _5], [_3, _6], [_7, _8]] =
  toDecimal $ fmap color [_8, _7, _6, _5, _4, _3, _2, _1]
 where
  toDecimal = foldl (\acc curr -> acc * 2 + curr) 0
  color = \case False -> 0; True -> 1
