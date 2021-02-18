module ChessBoard where

import ChessPieces
import ChessUtilTypes


-- -- Define Show for a ChessBoard
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
-- If the move results in a check-mate, sets the state of the ChessBoard to Over
-- WARNING: Assumes given move is valid!
-- TODO: implement + test this (1.5 hours) [Yiyi]
makeMove :: ChessBoard -> ChessMove -> ChessBoard
makeMove chessBoard move = chessBoard


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

-- Converts FEN to ChessBoard. TODO: Docs [Aziz]
fenToChessBoard forsythStr = (ChessBoard pieces Ongoing) where pieces = reverse (getFirst (foldl addPieces ([], 8, 0) forsythStr))
getFirst (a, b, c) = a
cols = ['A', 'B', 'C', 'D', 'E', 'F', 'G', 'H']
addPieces :: ([(ChessPosition, ChessPiece)], Int, Int) -> Char -> ([(ChessPosition, ChessPiece)], Int, Int)
addPieces (pieces, row, col) char
    | char=='/'     = (pieces, row-1, 0)
    | char=='p'     = addPiece row col pieces (Pawn Black) 
    | char=='P'     = addPiece row col pieces (Pawn White)
    | char=='n'     = addPiece row col pieces (Knight Black)
    | char=='N'     = addPiece row col pieces (Knight White)
    | char=='b'     = addPiece row col pieces (Bishop Black)
    | char=='B'     = addPiece row col pieces (Bishop White)
    | char=='r'     = addPiece row col pieces (Rook Black)
    | char=='R'     = addPiece row col pieces (Rook White)
    | char=='q'     = addPiece row col pieces (Queen Black)
    | char=='Q'     = addPiece row col pieces (Queen White)
    | char=='k'     = addPiece row col pieces (King Black)
    | char=='K'     = addPiece row col pieces (King White)
    | char=='8'     = (pieces, row, col) -- ignore 8s
    | otherwise     = (pieces, row, col + ((read::String->Int) [char]))
addPiece row col pieces newPiece = (newpieces, row, col+1) 
    where newpieces = ((cols !! col, row), newPiece):pieces


-- A fresh Chess Board with all the pieces in the starting position
freshBoard = fenToChessBoard "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR"