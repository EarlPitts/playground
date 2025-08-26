{-# LANGUAGE OverloadedStrings #-}

module Chess where

import Control.Monad
import Data.List
import qualified Data.Set as S
import Language.Javascript.JSaddle (jsg, (#))
import Data.Maybe
import Miso hiding (Off, focus, media_, set, update, Capture)
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
  , _currentPlayer :: Color
  , _validMoves :: [Position]
  }
  deriving (Show, Eq)

blacks :: Lens Model [Piece]
blacks = lens _blacks $ \m v -> m {_blacks = v}

whites :: Lens Model [Piece]
whites = lens _whites $ \m v -> m {_whites = v}

selected :: Lens Model (Maybe Piece)
selected = lens _selected $ \m v -> m {_selected = v}

player :: Lens Model Color
player = lens _currentPlayer $ \m v -> m {_currentPlayer = v}

validMoves :: Lens Model [Position]
validMoves = lens _validMoves $ \m v -> m {_validMoves = v}

initModel :: Model
initModel = Model bs ws Nothing White []
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

chess :: App Model Action
chess =
  (component initModel updateModel viewModel)
    {
      styles = [Sheet sheet]
    }

data Action
  = Select (Maybe Piece)
  | Move Piece Position
  | Capture Piece Piece
  deriving (Show)

updateModel :: Action -> Transition Model Action
updateModel (Select Nothing) = pure ()
updateModel (Select (Just piece)) = do
  c <- use player
  when ((pieceColor piece) == c) $ do
    selected .= Just piece
    ps <- liftA2 (<>) (use whites) (use blacks)
    validMoves .= filter (isValid piece ps) positions
updateModel (Move piece pos) = do
  case pieceColor piece of
    White -> whites %= fmap (movePiece piece pos)
    Black -> blacks %= fmap (movePiece piece pos)
  selected .= Nothing
  validMoves .= []
  player %= nextPlayer
updateModel (Capture capturer captured) = do
  case pieceColor capturer of
    White -> blacks %= delete captured
    Black -> whites %= delete captured
  issue (Move capturer (position captured))

-- TODO do something with this because it looks disgusting
isValid :: Piece -> [Piece] -> Position -> Bool
isValid (Piece White Pawn (Position r c)) ps (Position r' c') =
  if r == 6
  then ((r' == r - 1) || (r' == r - 2)) && c == c'
  else r' == r - 1 && c == c'
isValid (Piece Black Pawn (Position r c)) ps (Position r' c') =
  if r == 1
  then ((r' == r + 1) || (r' == r + 2)) && c == c'
  else r' == r + 1 && c == c'
  -- TODO diagonal capture
isValid (Piece col Knight (Position r c)) ps (Position r' c') =
  r' == r + 2 && c' == c + 1 ||
  r' == r - 2 && c' == c + 1 ||
  r' == r + 2 && c' == c - 1 ||
  r' == r - 2 && c' == c - 1 ||
  r' == r + 1 && c' == c + 2 ||
  r' == r - 1 && c' == c + 2 ||
  r' == r + 1 && c' == c - 2 ||
  r' == r - 1 && c' == c - 2
isValid (Piece col Queen (Position r c)) ps (Position r' c') =
  (r - c) == (r' - c') || (r + c) == (r' + c') ||
  r == r' || c == c'
isValid (Piece col King (Position r c)) ps pos'=
  pos' `elem` (Position <$> [r, succ r, pred r] <*> [c, succ c, pred c])
isValid (Piece col Rook (Position r c)) ps (Position r' c') =
  r == r' || c == c'
isValid (Piece col Bishop (Position r c)) ps (Position r' c') =
  (r - c) == (r' - c') || (r + c) == (r' + c')

nextPlayer :: Color -> Color
nextPlayer Black = White
nextPlayer White = Black

-- Either moves it if it's the right one, or
-- leaves it in place
movePiece :: Piece -> Position -> Piece -> Piece
movePiece p pos p' =
  if position p' == position p
  then (Piece (pieceColor p') (pieceType p') pos)
  else p'

positions :: [Position]
positions = [Position r c | r <- [0 .. 7], c <- [0 .. 7]]

board :: [Piece] -> Position -> Tile
board ps pos = Tile pos $ find (\(Piece _ _ p) -> p == pos) ps

viewModel :: Model -> View Model Action
viewModel (Model bs ws sel _ valids) =
  div_
    [class_ "grid-container"]
    [ div_
        [class_ "container"]
        (cellView sel valids <$> ((board $ bs <> ws) <$> positions))
    ]

isWhite :: Position -> Bool
isWhite (Position r c) =
  (odd r && odd c) ||
  (even r && even c)

isSelected :: Maybe Piece -> Position -> Bool
isSelected Nothing _ = False
isSelected (Just (Piece _ _ pos)) pos' = pos == pos'

cellView :: Maybe Piece -> [Position] -> Tile -> View Model Action
cellView sel valids (Tile pos piece) =
  div_
    ([ class_ classStr ] <> clickHandler)
    (maybe [] (singleton . text . ms . show) piece)
  where
    classStr = "grid-cell" <> 
      (if isSelected sel pos then " cell-selected" else "") <>
      (if (not $ isWhite pos) then " cell-black" else "") <>
      (if (pos `elem` valids) then " cell-selected" else "")
    clickHandler = case (sel, piece) of
      (Nothing, Nothing) -> []
      (Just s, Nothing) -> if pos `elem` valids
                           then [ onClick (Move s pos) ]
                           else []
      (Just s, Just p) -> if (pieceColor s == pieceColor p)
                          then [ onClick (Select (Just p)) ]
                          else if pos `elem` valids
                          then [ onClick (Capture s p) ]
                          else []
      (_, Just p) -> [ onClick (Select (Just p)) ]

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
