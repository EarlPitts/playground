{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Foldable
import qualified Data.Text as T
import qualified Data.Text.IO as T
import System.Environment

main :: IO ()
main = do
  [path, from, to] <- validateArgs <$> getArgs
  content <- T.readFile path
  let newContent = T.replace (T.pack from) (T.pack to) content
  T.putStrLn newContent

validateArgs :: [String] -> [String]
validateArgs args =
  if length args /= 3
    then error "Needs 3 parameters!"
    else args
