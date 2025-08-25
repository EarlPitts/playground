{-# LANGUAGE OverloadedStrings #-}

module Chess where

import Control.Monad
import Data.List
import qualified Data.Set as S
import Language.Javascript.JSaddle (jsg, (#))
import Data.Maybe
import Miso hiding (Off, focus, media_, set, update)
import Miso.Lens
import Miso.String (MisoString, ms)
import Miso.Style hiding (filter, ms, position, Color)
import Control.Applicative

data Color = White | Black deriving (Show, Eq)

data Position = Position Int Int deriving (Show, Eq)
data Piece = Piece { pieceColor :: Color, pieceType :: PieceType, position :: Position } deriving (Eq)
data PieceType = Pawn | Rook | Knight | Bishop | Queen | King deriving Eq
data Tile = Tile Position (Maybe Piece) deriving (Show, Eq)

instance Show Piece where
  show (Piece White King _)   = "♔"
  show (Piece Black King _)   = "♚"
  show (Piece White Queen _)  = "♕"
  show (Piece Black Queen _)  = "♛"
  show (Piece White Rook _)   = "♖"
  show (Piece Black Rook _)   = "♜"
  show (Piece White Bishop _) = "♗"
  show (Piece Black Bishop _) = "♝"
  show (Piece White Knight _) = "♘"
  show (Piece Black Knight _) = "♞"
  show (Piece White Pawn _)   = "♙"
  show (Piece Black Pawn _)   = "♟"

data Model = Model
  {
    _blacks :: [Piece]
  , _whites :: [Piece]
  , _selected :: Maybe Piece
  }
  deriving (Show, Eq)

blacks :: Lens Model [Piece]
blacks = lens _blacks $ \m v -> m {_blacks = v}

whites :: Lens Model [Piece]
whites = lens _whites $ \m v -> m {_whites = v}

selected :: Lens Model (Maybe Piece)
selected = lens _selected $ \m v -> m {_selected = v}

initModel :: Model
initModel = Model bs ws Nothing
  where
    bs = notPawns 0 Black <> pawns 1 Black
    ws = notPawns 7 White <> pawns 6 White

notPawns :: Int -> Color -> [Piece]
notPawns row c = getZipList $ Piece c <$> pieces <*> positions
  where
    positions = (Position <$> (ZipList (repeat row)) <*> (ZipList [0..7]))
    pieces = ZipList [Rook, Knight, Bishop, Queen, King, Bishop, Knight, Rook]

pawns :: Int -> Color -> [Piece]
pawns row c =
  (Piece c Pawn) <$>
  (getZipList (Position <$> (ZipList (repeat row)) <*> (ZipList [0..7])))

chessMain :: App Model Action
chessMain =
  (component initModel updateModel viewModel)
    {
      styles = [Sheet sheet]
    }

data Action
  = Select Piece
  | Move Piece
  deriving (Show)

updateModel :: Action -> Transition Model Action
updateModel (Select piece) = do
  selected .= Just piece
  m <- use selected
  io_ $ consoleLog (ms (show m))
updateModel (Move piece) = do
  selected .= Just piece
  m <- use selected
  io_ $ consoleLog (ms (show m))

positions :: [Position]
positions = [Position r c | r <- [0 .. 7], c <- [0 .. 7]]

board :: [Piece] -> Position -> Tile
board ps pos = Tile pos $ find (\(Piece _ _ p) -> p == pos) ps

viewModel :: Model -> View Model Action
viewModel (Model bs ws sel) =
  div_
    [class_ "grid-container"]
    [ div_
        [class_ "container"]
        (cellView sel <$> ((board $ bs <> ws) <$> positions))
    ]

isWhite :: Position -> Bool
isWhite (Position r c) =
  (odd r && odd c) ||
  (even r && even c)

isSelected :: Maybe Piece -> Position -> Bool
isSelected Nothing _ = False
isSelected (Just (Piece _ _ pos)) pos' = pos == pos'

cellView :: Maybe Piece -> Tile -> View Model Action
cellView sel (Tile pos piece) =
  div_
    ([ class_ classStr ] <> clickHandler)
    (maybe [] (singleton . text . ms . show) piece)
  where
    classStr = "grid-cell" <> 
      (if isSelected sel pos then " cell-selected" else "") <>
      (if (not $ isWhite pos) then " cell-black" else "")
    clickHandler = case (sel, piece) of
      (Nothing, Nothing) -> []
      (Just s, Nothing) -> [ onClick (Move s) ]
      (_ ,Just p) -> [ onClick (Select p) ]

sheet :: StyleSheet
sheet =
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
          gridTemplateColumns "repeat(8, 75px)",
          gridTemplateRows "repeat(8, 75px)",
          gap "0"
        ],
      selector_
        ".grid-cell"
        [ border "1px solid #333",
          height "75px",
          width "75px",
          boxSizing "border-box",
          display "flex",
          alignItems "center",
          justifyContent "center",
          fontSize "45px"
        ],
      selector_
        ".cell-black"
        [ background "black",
          color white
        ],
      selector_
        ".cell-selected"
        [ border "3px solid #f5da42",
          boxSizing "border-box"
        ]
    ]
