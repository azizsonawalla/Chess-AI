module ChessBoard where

import ChessPieces
import ChessUtilTypes


-- Define Show for a ChessBoard
instance Show ChessBoard where
    show board = chessBoardAsString board


-- Returns the string representation of a chessboard
-- TODO: implement + test this (2 hours).
-- Note: one way to implement this is to have a 'template' string (possibly stored in a txt file) and then simply
--       replace a placeholder in each square with the piece that is currently there.
chessBoardAsString :: ChessBoard -> [Char]
chessBoardAsString chessBoard = "<ASCII Representation of Chess Board>"


-- Makes the given move on the given chessboard. Returns the board with the move made
-- If there is a piece at the destination square, then that piece will be replaced
-- WARNING: Assumes given move is valid!
-- TODO: implement + test this (1.5 hours)
makeMove :: ChessBoard -> ChessMove -> ChessBoard
makeMove chessBoard move = chessBoard                 -- If the piece to move is a pawn, make sure to set it's boolean value to false


-- Returns all legal moves for the given side on the given chess board
-- TODO: implement + test this (1.5 hour).
-- Note: one way to implement is to iterate over all pieces of the given colour on the board and call legalMovesForPieceAtPos
legalMoves :: ChessBoard -> ChessPieceColour -> [ChessMove]
legalMoves chessBoard chessPieceColour = []


-- Returns true if the given move is valid on the given board, for the given colour
-- TODO: implement + test this (0.5 hour). You can use legalMoves above.
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
    , (('A', 2), Pawn White True)
    , (('B', 2), Pawn White True)
    , (('C', 2), Pawn White True)
    , (('D', 2), Pawn White True)
    , (('E', 2), Pawn White True)
    , (('F', 2), Pawn White True)
    , (('G', 2), Pawn White True)
    , (('H', 2), Pawn White True)
    , (('A', 8), Rook Black)
    , (('B', 8), Knight Black)
    , (('C', 8), Bishop Black)
    , (('D', 8), King Black)
    , (('E', 8), Queen Black)
    , (('F', 8), Bishop Black)
    , (('G', 8), Knight Black)
    , (('H', 8), Rook Black)
    , (('A', 7), Pawn Black True)
    , (('B', 7), Pawn Black True)
    , (('C', 7), Pawn Black True)
    , (('D', 7), Pawn Black True)
    , (('E', 7), Pawn Black True)
    , (('F', 7), Pawn Black True)
    , (('G', 7), Pawn Black True)
    , (('H', 7), Pawn Black True) ] 
    Ongoing