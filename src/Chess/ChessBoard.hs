module ChessBoard where

import ChessPieces
import ChessUtilTypes


-- Define Show for a ChessBoard
instance Show ChessBoard where
    show board = chessBoardAsString board


-- Returns the string representation of a chessboard
-- TODO: implement this
chessBoardAsString :: ChessBoard -> [Char]
chessBoardAsString chessBoard = "<ASCII Representation of Chess Board>"


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
getPieceAt :: ChessPosition -> ChessBoard -> Maybe ChessPiece
getPieceAt position (ChessBoard pieces _) = lookup position pieces

-- Returns true if the chessboard has been closed to indicate the game is over
gameOver (ChessBoard _ state) = state == Over


-- A fresh Chess Board with all the pieces in the starting position
freshBoard = ChessBoard 
    [ (('A', 1), Rook White)
    , (('B', 1), Knight White)
    , (('C', 1), Bishop White)
    , (('D', 1), King White)
    , (('E', 1), Queen White)
    , (('F', 1), Bishop White)
    , (('G', 1), Knight White)
    , (('H', 1), Rook White)
    , (('A', 2), Pawn White)
    , (('B', 2), Pawn White)
    , (('C', 2), Pawn White)
    , (('D', 2), Pawn White)
    , (('E', 2), Pawn White)
    , (('F', 2), Pawn White)
    , (('G', 2), Pawn White)
    , (('H', 2), Pawn White)
    , (('A', 8), Rook Black)
    , (('B', 8), Knight Black)
    , (('C', 8), Bishop Black)
    , (('D', 8), King Black)
    , (('E', 8), Queen Black)
    , (('F', 8), Bishop Black)
    , (('G', 8), Knight Black)
    , (('H', 8), Rook Black)
    , (('A', 7), Pawn Black)
    , (('B', 7), Pawn Black)
    , (('C', 7), Pawn Black)
    , (('D', 7), Pawn Black)
    , (('E', 7), Pawn Black)
    , (('F', 7), Pawn Black)
    , (('G', 7), Pawn Black)
    , (('H', 7), Pawn Black) ] 
    Ongoing