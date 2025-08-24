{-# LANGUAGE OverloadedStrings #-}

module Chess where

import Control.Monad
import Data.List
import qualified Data.Set as S
import Language.Javascript.JSaddle (jsg, (#))
import Miso hiding (Off, focus, media_, set, update)
import Miso.Lens
import Miso.String (MisoString, ms)
import Miso.Style hiding (filter, ms, position)
import Control.Applicative

-- data Player = White | Black deriving (Show, Eq, MisoString)

data Position = Position Int Int deriving (Show, Eq)
data Piece = Piece PieceType Position deriving (Show, Eq)
data PieceType = Pawn | Rook | Knight | Bishop | Queen | King deriving (Show, Eq)
data Tile = Tile Position (Maybe Piece) deriving (Show, Eq)

data Model = Model
  {
    _blacks :: [Piece]
  , _whites :: [Piece]
  }
  deriving (Show, Eq)

blacks :: Lens Model [Piece]
blacks = lens _blacks $ \m v -> m {_blacks = v}

whites :: Lens Model [Piece]
whites = lens _whites $ \m v -> m {_whites = v}

initModel :: Model
initModel = Model bs ws
  where
    bs = notPawns 0 <> pawns 1
    ws = notPawns 7 <> pawns 6

notPawns :: Int -> [Piece]
notPawns row = getZipList $ Piece <$> pieces <*> positions
  where
    positions = (Position <$> (ZipList (repeat row)) <*> (ZipList [0..7]))
    pieces = ZipList [Rook, Knight, Bishop, Queen, King, Bishop, Knight, Rook]

pawns :: Int -> [Piece]
pawns row =
  (Piece Pawn) <$>
  (getZipList (Position <$> (ZipList (repeat row)) <*> (ZipList [0..7])))

chessMain :: App Model Action
chessMain =
  (component initModel updateModel viewModel)
    {
      styles = [Sheet sheet]
    }

data Action
  = None
  deriving (Show)

updateModel :: Action -> Transition Model Action
updateModel None = pure ()

positions :: [Position]
positions = [Position r c | r <- [0 .. 7], c <- [0 .. 7]]

board :: [Piece] -> Position -> Tile
board ps pos = Tile pos $ find (\(Piece _ p) -> p == pos) ps

viewModel :: Model -> View Model Action
viewModel (Model bs ws) =
  div_
    [class_ "grid-container"]
    [ div_
        [class_ "container"]
        (cellView <$> ((board $ bs <> ws) <$> positions))
    ]

isWhite :: Position -> Bool
isWhite (Position r c) =
  (odd r && odd c) ||
  (even r && even c)

cellView ::Tile -> View Model Action
cellView (Tile pos Nothing) = if isWhite pos
  then div_ [class_ "grid-cell-off"] []
  else div_ [class_ "grid-cell-on"] []
cellView (Tile pos (Just (Piece t _))) = if isWhite pos
  then div_ [class_ "grid-cell-off"] [ text (ms (show t)) ]
  else div_ [class_ "grid-cell-on"] [ text (ms (show t)) ]

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
        ".grid-cell-off"
        [ border "1px solid #333",
          height "75px",
          width "75px",
          textAlign "center"
        ],
      selector_
        ".grid-cell-on"
        [ border "1px solid #333",
          height "75px",
          width "75px",
          background "black",
          color white,
          textAlign "center"
        ]
    ]
