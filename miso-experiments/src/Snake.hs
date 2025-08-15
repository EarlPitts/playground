{-# LANGUAGE OverloadedStrings #-}

module Snake where

import Grid
import Data.List
import System.Random
import qualified Data.Set as S
import Control.Monad
import Miso hiding (media_, focus, update, set, Off)
import Miso.Lens
import Miso.String (ms, toMisoString, ToMisoString)
import Miso.Style hiding (ms, filter, position)

data Dir = U | D | L | R deriving (Show, Eq)

data Snake = Snake Dir [Position] deriving (Show, Eq)
data Fruits = Fruits [Position] deriving (Show, Eq)
data Tile = On | Off

type Size = Int

board :: Snake -> Fruits -> Position -> Tile
board (Snake _ ps) (Fruits fs) p = if p `elem` (ps <> fs) then On else Off

step :: Snake -> Snake
step (Snake d (h : t)) = Snake d ((move h d) : h : init t)
step _ = error "shouldn't happen"

randPos :: Snake -> Size -> Position
randPos (Snake _ ps) size = Position (mod (abs x) size) (mod (abs y) size)
  where
    ((x, y), _) = uniform (mkStdGen n)
    n = sum $ (\(Position r c) -> r + c) <$> ps

changeDir :: Dir -> Snake -> Snake
changeDir dir (Snake _ ps) = Snake dir ps

onFruit :: Snake -> Fruits -> Bool
onFruit (Snake _ (h:_)) (Fruits fs) = h `elem` fs
onFruit (Snake _ []) _ = error "shouldn't happen"

munch :: Snake -> Fruits -> (Snake, Fruits)
munch (Snake d (h:t)) (Fruits fs) =
  (Snake d (h:h:t), Fruits (delete h fs))
munch _ _ = error "shouldn't happen"

move :: Position -> Dir -> Position
move (Position r c) U = Position (r - 1) c
move (Position r c) D = Position (r + 1) c
move (Position r c) L = Position r (c - 1)
move (Position r c) R = Position r (c + 1)

addFruit :: Position -> Fruits -> Fruits
addFruit p (Fruits fs) = Fruits (p : fs)

initModel :: Int -> Model
initModel size = Model size (Snake R [Position (mid + 1) mid, Position mid mid, Position (mid-1) mid]) (Fruits []) False 0
  where
    mid = div size 2

keyMaps :: ([Int], [Int], [Int], [Int])
keyMaps = (
  [87, 38, 75], -- up
  [83, 40, 74], -- down
  [65, 37, 72], -- left
  [68, 39, 76]  -- right
  )

snakeMain :: App Model Action
snakeMain =
  (component (initModel size) updateModel viewModel)
    { events = pointerEvents,
      subs = [directionSub keyMaps GetArrows, keyboardSub GetKeys],
      styles = [Sheet (sheet size)]
    }
  where
    size = 15

data Model = Model {
   _boardSize :: Size
  ,_snake :: Snake
  ,_fruits :: Fruits
  ,_isRunning :: Bool
  ,_ticks :: Int
  }
  deriving (Show, Eq)

-- instance ToMisoString Model where
--   toMisoString (Model _ s _ _ _) = toMisoString (show s)

snakeL :: Lens Model Snake
snakeL = lens _snake $ \m v -> m {_snake = v}

fruitsL :: Lens Model Fruits
fruitsL = lens _fruits $ \m v -> m {_fruits = v}

boardSizeL :: Lens Model Size
boardSizeL = lens _boardSize $ \m v -> m {_boardSize = v}

isRunningL :: Lens Model Bool
isRunningL = lens _isRunning $ \m v -> m {_isRunning = v}

ticksL :: Lens Model Int
ticksL = lens _ticks $ \m v -> m {_ticks = v}

data Action
  = Step
  | Run
  | GetArrows Arrows
  | GetKeys (S.Set Int)
  deriving (Show)

togglePause :: Transition Model Action
togglePause = isRunningL %= not

incrTick :: Transition Model Action
incrTick = ticksL += 1

updateModel :: Action -> Transition Model Action
updateModel Run = use isRunningL >>= (flip when) (batch [pure Step, pure Run])
updateModel (GetKeys keys) = when (S.member 32 keys) $ togglePause
updateModel (GetArrows a) = handleDirChange a
updateModel Step = handleStep

handleDirChange :: Arrows -> Transition Model Action
handleDirChange a = case (getDir a) of
    Just dir -> do
      snakeL %= changeDir dir
      running <- use isRunningL
      unless running (isRunningL .= True >> (io $ pure Run))
    Nothing -> pure ()

feedingTime :: Int -> Bool
feedingTime ticks = (mod ticks 30) == 0

eat :: Transition Model Action
eat = do
  s <- use snakeL
  fs <- use fruitsL
  when (onFruit s fs) $ do
    let (newS, newFs) = munch s fs
    snakeL .= newS
    fruitsL .= newFs

feed :: Transition Model Action
feed = do
    size <- use boardSizeL
    snake <- use snakeL
    let p = randPos snake size
    fruitsL %= addFruit p

handleStep :: Transition Model Action
handleStep = do
  incrTick
  snakeL %= step
  eat
  t <- use ticksL
  when (feedingTime t) feed

getDir :: Arrows -> Maybe Dir
getDir (Arrows 1 0) = Just R
getDir (Arrows (-1) 0) = Just L
getDir (Arrows 0 1) = Just U
getDir (Arrows 0 (-1)) = Just D
getDir _ = Nothing

positions :: Size -> [Position]
positions s = [ Position r c | r <- [0..s-1], c <- [0..s-1] ]

viewModel :: Model -> View Model Action
viewModel (Model size snake fruits _ _) =
  div_
    [class_ "grid-container"]
    [
      div_
        [class_ "container"]
        (cellView <$> ((board snake fruits) <$> positions size))
    ]

cellView :: Tile -> View Model Action
cellView Off = div_ [class_ "grid-cell-off"] []
cellView On = div_ [class_ "grid-cell-on"] []

sheet :: Int -> StyleSheet
sheet size =
  sheet_
    [ selector_
        ".grid-container"
        [ display "grid",
          justifyContent "center",
          alignItems "center",
          height "100vh"
        ],
      selector_
        ".container"
        [ display "grid",
          gridTemplateColumns (ms $ "repeat(" <> (show size) <> ", 40px)"),
          gridTemplateRows (ms $ "repeat(" <> (show size) <> ", 40px)"),
          gap "0"
        ],
      selector_
        ".grid-cell-off"
        [ border "1px solid #333",
          height "40px",
          width "40px"
        ],
      selector_
        ".grid-cell-on"
        [ border "1px solid #333",
          height "40px",
          width "40px",
          background "black",
          color white,
          textAlign "center"
        ]
    ]
