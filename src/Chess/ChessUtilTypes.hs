module ChessUtilTypes where
import Data.List (sort)
import Data.Char
import Util

{- !!All data types are in this module to avoid cyclic imports!! -}


-- A chess player
-- Takes a chess board + chess colour, makes a move for that colour, and returns a new chessboard
type MoveFunction = (ChessPieceColour -> ChessBoard -> IO (ChessBoard, ChessMove))
data ChessPlayer = ChessPlayer String ChessPieceColour MoveFunction


-- A position on the Chess board
-- Uses algebraic notation for rows and columns: https://en.wikipedia.org/wiki/Algebraic_notation_(chess)
type ChessPosition = (Char, Integer)
chessPositionToString (char, num) = char:(show num)

chessPosFromStr :: String -> ChessPosition
chessPosFromStr posStr = ((posStrUpper !! 0), toInteger (digitToInt (posStrUpper !! 1))) 
  where posStrUpper = (toUpperStr posStr)


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


-- A tree representing all possible outcomes starting from the root chessboard
-- All scores are initially -1
type Score = Integer
data MoveSubtree = MoveSubtree ChessMove GameTree deriving (Ord)              -- a legal move from root and the resulting subtree
data GameTree = GameTree ChessBoard Score [MoveSubtree]
instance Eq GameTree where -- 2 GameTrees are equal if the roots are equal and the children are pairwise equal
    (GameTree c1 s1 m1) == (GameTree c2 s2 m2) = (c1 == c2) && (s1 == s2) && sameNumOfChildren && childrenAreSame
        where sameNumOfChildren = (length m1) == (length m2)
              childrenAreSame = foldr (\ (child1, child2) acc -> acc && (child1 == child2)) True zippedChildren
              zippedChildren = zip (sort m1) (sort m2)