module Mandelbrot where

import Data.Char
import Data.Complex
import Data.Foldable (traverse_)
import Data.List.Split
import System.Console.Terminal.Size (Window (..))
import qualified System.Console.Terminal.Size as Term

main :: IO ()
main = do
  Just (Window h w) <- Term.size
  traverse_ putStrLn $ render (mandelbrot (w * 2) (h * 4 - 2)) w (h - 2)

type Cell = [[Bool]] -- 2x8
type Canvas = [[Bool]] -- arbitrary size

circle :: Int -> (Int, Int) -> Bool
circle radius (x, y) = (x - 70) ^ 2 + (y - 80) ^ 2 <= radius

mandelbrot :: Int -> Int -> (Int, Int) -> Bool
mandelbrot w h (x, y) =
  let
    centerX = -0.75
    centerY = 0.1
    zoom = 6
    f c z = z * z + c
    nx = centerX + (fromIntegral x / fromIntegral w - 0.5) * 3.5 / zoom -- - 2.5
    -- ny = fromIntegral y / fromIntegral h * 2.0 - 1.0
    ny = centerY + (fromIntegral y / fromIntegral h - 0.5) * 2.0 / zoom
    iters = 80
    values = iterate (f (nx :+ ny)) 0
   in
    2 > magnitude (values !! iters)

render :: ((Int, Int) -> Bool) -> Int -> Int -> [String]
render f width height =
  let
    canvas = [(x, y) | y <- [0 .. height * 4 - 1], x <- [0 .. width * 2 - 1]]
   in
    (fmap . fmap) renderCell $ discretize $ chunksOf (width * 2) (fmap f canvas)

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
