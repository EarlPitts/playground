module Main where

import Life
import Snake
import Miso

main :: IO ()
main = run $ startComponent snakeMain
