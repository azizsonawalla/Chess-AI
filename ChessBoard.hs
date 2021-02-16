module ChessBoard where

import ChessPieces

-- A position on the Chess board
-- Uses algebraic notation for rows and columns: https://en.wikipedia.org/wiki/Algebraic_notation_(chess)
type ChessPosition = (Char, Int)

-- A Chess move from one position to the other
data ChessMove = ChessMove ChessPosition ChessPosition  -- ChessMove from to

-- A Chess Board
-- It comprises of a list of 2-tupes - a position on the board and the corresponding piece at that position
-- If there is no tuple for a particular position, then there is no piece there
data ChessBoard = ChessBoard [(ChessPosition, ChessPiece)]

-- Define Show for a ChessBoard
instance Show ChessBoard where
    show board = chessBoardAsString board

-- Returns the string representation of a chessboard
-- TODO: implement this
chessBoardAsString :: ChessBoard -> [Char]
chessBoardAsString chessBoard = "Not implemented"

-- Makes the given move on the given chessboard.
-- Returns the board with the move made
-- WARNING: Assumes given move is valid!
-- TODO: implement this
makeMove :: ChessBoard -> ChessMove -> ChessBoard
makeMove chessBoard move = chessBoard

-- Returns true if the given move is valid on the given board
-- TODO: implement this
validMove :: ChessBoard -> ChessMove -> Bool
validMove _ _ = False

-- Returns true if the chessboard has been closed to indicate the game is over
-- TODO: Implement this
gameOver chessboard = False

-- A fresh Chess Board with all the pieces in the starting position
-- TODO: Implement this
freshBoard = ChessBoard []