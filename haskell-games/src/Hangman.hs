{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Hangman where

import Brick
import qualified Brick.AttrMap as A
import qualified Brick.Main as M
import qualified Brick.Types as T
import Brick.Widgets.Center (center)
import Brick.Widgets.Table
import Control.Monad (void)
-- import Network.HTTP.Req

import Control.Monad.IO.Class
import Data.Aeson (Value (..))
import Data.Aeson.Decoding
import Data.Bifunctor
import Data.List
import Data.Text (Text, unpack)
import qualified Graphics.Vty as V
import Lens.Micro.Mtl
import Lens.Micro.TH
import Network.HTTP.Req
import System.Random
import qualified Data.Vector as VEC
import Data.Aeson.Types (Array)

data AppState = AppState
  { _word :: String,
    _guessed :: [Char],
    _gen :: StdGen
  }
  deriving (Show, Eq)

makeLenses ''AppState

getLetters :: String -> [Char]
getLetters = nub . sort

showGuessed :: String -> [Char] -> String
showGuessed w ls =
  intersperse ' ' $ (\c -> if c `elem` ls then c else '_') <$> w

initialState :: String -> StdGen -> AppState
initialState w g =
  let letternum = div (length w) 4
      (guessed, _) =
        first
          (take letternum)
          (uniformShuffleList (getLetters w) g)
   in AppState w guessed g

appEvent :: T.BrickEvent () e -> T.EventM () AppState ()
appEvent (T.VtyEvent e) = case e of
  V.EvKey (V.KChar c) [] -> guess c
  V.EvKey V.KEsc [] -> M.halt
  V.EvKey (V.KChar 'q') [] -> M.halt
  _ -> return ()
appEvent _ = return ()

guess :: Char -> T.EventM () AppState ()
guess c = do
  w <- use word
  if c `elem` w
    then guessed %= (c :)
    else pure ()

theMap :: A.AttrMap
theMap = A.attrMap V.defAttr []

drawUI :: AppState -> [Widget ()]
drawUI s = [center $ str (showGuessed (s._word) (s._guessed))]

theApp :: M.App AppState e ()
theApp =
  M.App
    { M.appDraw = drawUI,
      M.appChooseCursor = M.showFirstCursor,
      M.appHandleEvent = appEvent,
      M.appStartEvent = return (),
      M.appAttrMap = const theMap
    }

main :: IO ()
main = do
  word <- getWord 
  gen <- initStdGen
  void $ M.defaultMain theApp (initialState word gen)

getWord :: IO String
getWord = runReq defaultHttpConfig $ do
  r <-
    req
      GET
      (https "random-word-api.herokuapp.com" /: "word")
      NoReqBody
      bsResponse
      mempty
  let (Just [word]) = decodeStrict (responseBody r)
  liftIO $ pure word
