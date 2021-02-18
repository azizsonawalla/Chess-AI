module ChessUtilTypes where
import Data.List (sort)

{- !!All data types are in this module to avoid cyclic imports!! -}


-- A chess player
-- Takes a chess board + chess colour, makes a move for that colour, and returns a new chessboard
type ChessPlayer = (ChessPieceColour -> ChessBoard -> IO ChessBoard)


-- A position on the Chess board
-- Uses algebraic notation for rows and columns: https://en.wikipedia.org/wiki/Algebraic_notation_(chess)
type ChessPosition = (Char, Int)
chessPositionToString (char, num) = char:(show num)


-- A Chess move from one position to the other
data ChessMove = ChessMove ChessPosition ChessPosition deriving (Eq)  -- ChessMove from to
instance Show ChessMove where
  show (ChessMove start end) = (chessPositionToString start)++" to "++(chessPositionToString end)


-- The current state of a chess game
data GameState = Ongoing | Over deriving (Eq, Show)


-- A Chess Board
-- It comprises of a list of 2-tupes - a position on the board and the corresponding piece at that position
-- If there is no tuple for a particular position, then there is no piece there
data ChessBoard = ChessBoard [(ChessPosition, ChessPiece)] GameState -- deriving (Show)
instance Eq ChessBoard where
  (ChessBoard pieces1 state1) == (ChessBoard pieces2 state2) = (state1 == state2) && ((sort pieces1) == (sort pieces2))


-- The two sides/colours on a chess board
data ChessPieceColour = White | Black deriving (Eq, Show)


-- All pieces on a chess board
data ChessPiece = 
      King ChessPieceColour
    | Queen ChessPieceColour
    | Rook ChessPieceColour
    | Bishop ChessPieceColour
    | Knight ChessPieceColour
    | Pawn ChessPieceColour
     deriving (Eq, Show)
instance Ord ChessPiece where
  piece1 <= piece2 = (show piece1) <= (show piece2)