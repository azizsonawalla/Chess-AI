module ChessBoard where

import ChessPieces
import ChessUtilTypes


-- Define Show for a ChessBoard
instance Show ChessBoard where
    show board = chessBoardAsString board


-- Returns the string representation of a chessboard
-- TODO: implement + test this (2 hours). [Cynthia]
-- Note: one way to implement this is to have a 'template' string (possibly stored in a txt file) and then simply
--       replace a placeholder in each square with the piece that is currently there.
chessBoardAsString :: ChessBoard -> [Char]
chessBoardAsString chessBoard = "<ASCII Representation of Chess Board>"


-- Makes the given move on the given chessboard. Returns the board with the move made
-- If there is a piece at the destination square, then that piece will be replaced
-- WARNING: Assumes given move is valid!
-- TODO: implement + test this (1.5 hours) [Yiyi]
makeMove :: ChessBoard -> ChessMove -> ChessBoard
makeMove chessBoard move = chessBoard                 -- If the piece to move is a pawn, make sure to set it's boolean value to false


-- Returns all legal moves for the given side on the given chess board
-- TODO: test this [Aziz] -- waiting for legalNextPosForPieceAtPos implementations
legalMoves :: ChessBoard -> ChessPieceColour -> [ChessMove]
legalMoves chessBoard chessPieceColour = foldr (\ (position, piece) allMoves -> allMoves ++ (legalMovesForPieceAtPos piece chessBoard position)) [] filteredPieces
    where (ChessBoard filteredPieces state) = filterChessBoard chessBoard chessPieceColour


-- Returns true if the given move is valid on the given board, for the given colour
-- TODO: implement + test this (0.5 hour). You can use legalMoves above. [Yiyi]
validMove :: ChessBoard -> ChessPieceColour -> ChessMove -> Bool
validMove _ _ _ = False


-- Returns the chess piece at the given position as a Maybe
getPieceAt :: ChessPosition -> ChessBoard -> Maybe ChessPiece
getPieceAt position (ChessBoard pieces _) = lookup position pieces

-- Returns true if the chessboard has been closed to indicate the game is over
gameOver (ChessBoard _ state) = state == Over


-- Returns a version of the chessboard with only the pieces of the given colour
filterChessBoard :: ChessBoard -> ChessPieceColour -> ChessBoard
filterChessBoard (ChessBoard ogPieces state) colour = ChessBoard (filter (\ (position, piece) -> (getPieceColour piece) == colour) ogPieces) state


-- A fresh Chess Board with all the pieces in the starting position
freshBoard = ChessBoard 
    [ (('A', 1), Rook White)
    , (('B', 1), Knight White)
    , (('C', 1), Bishop White)
    , (('D', 1), Queen White)
    , (('E', 1), King White)
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
    , (('D', 8), Queen Black)
    , (('E', 8), King Black)
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