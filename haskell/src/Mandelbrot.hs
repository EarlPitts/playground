module Mandelbrot where

import Data.Char
import Data.Foldable (traverse_)
import System.Console.Terminal.Size (Window (..))
import qualified System.Console.Terminal.Size as Term

main :: IO ()
main = do
  Just (Window h w) <- Term.size
  let canvas = replicate (h * 4) (replicate (w * 2) White)
  traverse_ putStrLn $ render canvas

data Dot = Black | White deriving (Show, Eq)

type Cell = [[Dot]] -- 2x8
type Canvas = [[Dot]] -- arbitrary size

render :: Canvas -> [String]
render = (fmap . fmap) renderCell . discretize

discretize :: Canvas -> [[Cell]]
discretize [] = []
discretize (r1 : r2 : r3 : r4 : rs) = f r1 r2 r3 r4 : discretize rs
 where
  f [] [] [] [] = []
  f (_1 : _4 : cs) (_2 : _5 : cs') (_3 : _6 : cs'') (_7 : _8 : cs''') = [[[_1, _4], [_2, _5], [_3, _6], [_7, _8]]] <> f cs cs' cs'' cs'''

renderCell :: Cell -> Char
renderCell cell = chr $ 0x2800 + fromIntegral (toDecimal cell)

toDecimal :: Cell -> Int
toDecimal [[_1, _4], [_2, _5], [_3, _6], [_7, _8]] =
  foldl (\acc curr -> acc * 2 + curr) 0 $ fmap (\case Black -> 0; White -> 1) (reverse [_1, _2, _3, _4, _5, _6, _7, _8])

canvas =
  [ [Black, White, White, Black, Black, White]
  , [Black, White, White, Black, Black, White]
  , [Black, White, White, Black, Black, White]
  , [Black, White, White, Black, Black, White]
  ]
