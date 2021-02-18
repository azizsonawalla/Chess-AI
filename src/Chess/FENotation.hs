module FENotation where

import Util
import ChessUtilTypes

-- Forsyth Edwards Notation - https://en.wikipedia.org/wiki/Forsyth%E2%80%93Edwards_Notation


-- Converts a chess board configuration in FEN notation to ChessBoard type.
fenToChessBoard :: String -> ChessBoard
fenToChessBoard fenStr = (ChessBoard pieces Ongoing) where pieces = reverse (getFirst (foldl addPieces ([], 8, 0) fenStr))


-- The columns of a Chess Board
chessBoardCols :: [Char]
chessBoardCols = ['A', 'B', 'C', 'D', 'E', 'F', 'G', 'H']


-- Adds the corresponding ChessPieces to the accumulator based on the char from a FEN string
-- Also increments row and col accordingly
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


-- Adds the given piece to the accumulator
addPiece :: Int -> Int -> [(ChessPosition, ChessPiece)] -> ChessPiece -> ([(ChessPosition, ChessPiece)], Int, Int)
addPiece row col pieces newPiece = (newpieces, row, col+1) 
    where newpieces = ((chessBoardCols !! col, row), newPiece):pieces