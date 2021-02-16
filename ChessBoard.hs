module ChessBoard where

import ChessPieces
import ChessUtilTypes


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


-- Returns all legal moves for the given side on the given chess board
-- TODO: implement this
legalMoves :: ChessBoard -> ChessPieceColour -> [ChessMove]
legalMoves chessBoard chessPieceColour = []


-- Returns true if the given move is valid on the given board, for the given colour
-- TODO: implement this
validMove :: ChessBoard -> ChessPieceColour -> ChessMove -> Bool
validMove _ _ _ = False


-- Returns the chess piece at the given position as a Maybe
-- TODO: test this
getPieceAt :: ChessPosition -> ChessBoard -> Maybe ChessPiece
getPieceAt position (ChessBoard pieces _) = lookup position pieces

-- Returns true if the chessboard has been closed to indicate the game is over
-- TODO: test this
gameOver (ChessBoard _ state) = state == Over


-- A fresh Chess Board with all the pieces in the starting position
-- TODO: Implement this
freshBoard = ChessBoard [] Ongoing