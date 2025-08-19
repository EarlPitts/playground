module Main where

import Life
import Todo
import Snake
import Miso
import TicTacToe

main :: IO ()
main = run $ startComponent ticTacToeApp
