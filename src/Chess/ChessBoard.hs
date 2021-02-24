module ChessBoard where

import ChessPieces
import ChessUtilTypes
import FENotation
import Data.Maybe


-- -- Define Show for a ChessBoard
instance Show ChessBoard where
    show board = chessBoardAsString board

-- A fresh Chess Board with all the pieces in the starting position
freshBoard = fenToChessBoard "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR"

-- Returns the string representation of a chessboard
chessBoardAsString :: ChessBoard -> [Char]
chessBoardAsString chessBoard = 
                        "\n\n      (A)  (B)  (C)  (D)  (E)  (F)  (G)  (H) \n\n" 
                        ++ (foldl (\boardStr rowNum -> boardStr ++ (chessBoardRowAsString rowNum chessBoard)) "" [8, 7, 6, 5, 4, 3, 2, 1])
                        ++ "\n      (A)  (B)  (C)  (D)  (E)  (F)  (G)  (H) \n\n"   
                        ++ "\n" 

chessBoardRowAsString :: Integer -> ChessBoard -> String
chessBoardRowAsString rowNum chessBoard = rowNumStr ++ "  " ++ (foldl (\ rowStr position -> rowStr ++ (getPieceAsString position chessBoard)) "" [(col, rowNum) | col <- "ABCDEFGH"]) ++ "  " ++ rowNumStr ++ "\n"
    where rowNumStr = "(" ++ (show rowNum) ++ ")"

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
-- TODO: test this (0.5 hour). [Yiyi]
validMove :: ChessBoard -> ChessPieceColour -> ChessMove -> Bool
validMove board colour move = elem move (legalMoves board colour)


-- Returns true if the chessboard has been closed to indicate the game is over
gameOver (ChessBoard _ state) = state == Over


-- Returns a version of the chessboard with only the pieces of the given colour
filterChessBoard :: ChessBoard -> ChessPieceColour -> ChessBoard
filterChessBoard (ChessBoard ogPieces state) colour = ChessBoard (filter (\ (position, piece) -> (getPieceColour piece) == colour) ogPieces) state