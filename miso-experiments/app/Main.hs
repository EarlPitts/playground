module Main where

import Life
import Todo
import Snake
import Miso
import TicTacToe
import Chess

main :: IO ()
main = run $ startComponent chess
