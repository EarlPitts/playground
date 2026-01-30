{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Hangman where

import Brick
import qualified Brick.AttrMap as A
import qualified Brick.Main as M
import qualified Brick.Types as T
import Brick.Widgets.Center (center)
import Control.Monad (void, when)
import Control.Monad.IO.Class
import Data.Aeson.Decoding
import Data.Bifunctor
import Data.List
import qualified Data.Set as S
import qualified Graphics.Vty as V
import Lens.Micro.Mtl
import Lens.Micro.TH
import Network.HTTP.Req
import System.Random

data AppState = AppState
  { _word :: String,
    _guessed :: S.Set Char,
    _wrong :: S.Set Char,
    _gen :: StdGen,
    _ended :: Bool
  }
  deriving (Show, Eq)

makeLenses ''AppState

getLetters :: String -> [Char]
getLetters = nub . sort

showGuessed :: String -> S.Set Char -> String
showGuessed w ls =
  intersperse ' ' $ (\c -> if c `elem` ls then c else '_') <$> w

initialState :: String -> StdGen -> AppState
initialState w g =
  let letternum = div (length w) 4
      (guessed, _) =
        first
          (S.fromList . take letternum)
          (uniformShuffleList (getLetters w) g)
   in AppState w guessed S.empty g False

appEvent :: T.BrickEvent () e -> T.EventM () AppState ()
appEvent (T.VtyEvent e) = case e of
  V.EvKey (V.KChar ' ') [] -> newGame
  V.EvKey (V.KChar c) [] -> use ended >>= (flip when) (guess c) . not
  V.EvKey V.KEsc [] -> M.halt
  _ -> return ()
appEvent _ = return ()

newGame :: T.EventM () AppState ()
newGame = do
  (AppState _ _ _ gen ended) <- get
  if ended
    then do
      newWord <- liftIO getWord
      put $ initialState newWord gen
    else pure ()

guess :: Char -> T.EventM () AppState ()
guess c = do
  w <- use word
  if c `elem` w
    then do
      g <- use guessed
      let newGuessed = S.insert c g
      guessed .= newGuessed
      when
        ((S.fromList $ getLetters w) == newGuessed)
        (ended .= True)
    else do
      ws <- use wrong
      let newWrong = S.insert c ws
      wrong .= newWrong
      when
        (length newWrong > 5)
        ((guessed .= S.fromList w) >> (ended .= True))

theMap :: A.AttrMap
theMap = A.attrMap V.defAttr []

drawUI :: AppState -> [Widget ()]
drawUI s =
  [ center $
      drawHanging (s._wrong)
        <+> ( padLeft
                (Pad 3)
                ( padTop
                    (Pad 2)
                    ( (str (showGuessed s._word s._guessed))
                        <=> (str $ S.toList s._wrong)
                    )
                )
            )
  ]

drawHanging :: S.Set Char -> Widget ()
drawHanging ws
  | S.size ws == 0 = str start
  | S.size ws <= 1 = str head
  | S.size ws <= 3 = str half
  | S.size ws <= 5 = str arms
  | otherwise = str full
  where
    start :: String
    start =
      "\n\
      \      _ _ _\n\
      \      |\n\
      \      |\n\
      \      |\n\
      \      |\n\
      \     =========\n"

    head :: String
    head =
      "\n\
      \      _ _ _\n\
      \      |    O\n\
      \      |\n\
      \      |\n\
      \      |\n\
      \     =========\n"

    half :: String
    half =
      "\n\
      \      _ _ _\n\
      \      |    O\n\
      \      |    |\n\
      \      |\n\
      \      |\n\
      \     =========\n"

    arms :: String
    arms =
      "\n\
      \      _ _ _\n\
      \      |    O\n\
      \      |   /|\\\n\
      \      |\n\
      \      |\n\
      \     =========\n"

    full :: String
    full =
      "\n\
      \      _ _ _\n\
      \      |    O\n\
      \      |   /|\\\n\
      \      |   / \\\n\
      \      |\n\
      \     =========\n"

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
