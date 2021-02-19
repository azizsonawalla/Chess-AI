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
                        "\n    *--*--*   Current Chess Board   *--*--*\n\n"
                        ++ "      (A)  (B)  (C)  (D)  (E)  (F)  (G)  (H) \n\n" 
                        ++ "(8)  "  ++ getPieceAsString ('A',8) chessBoard
                                    ++ getPieceAsString ('B',8) chessBoard 
                                    ++ getPieceAsString ('C',8) chessBoard 
                                    ++ getPieceAsString ('D',8) chessBoard 
                                    ++ getPieceAsString ('E',8) chessBoard 
                                    ++ getPieceAsString ('F',8) chessBoard 
                                    ++ getPieceAsString ('G',8) chessBoard 
                                    ++ getPieceAsString ('H',8) chessBoard 
                        ++ "\n" 
                        ++ "(7)  "  ++ getPieceAsString ('A',7) chessBoard
                                    ++ getPieceAsString ('B',7) chessBoard 
                                    ++ getPieceAsString ('C',7) chessBoard 
                                    ++ getPieceAsString ('D',7) chessBoard 
                                    ++ getPieceAsString ('E',7) chessBoard 
                                    ++ getPieceAsString ('F',7) chessBoard 
                                    ++ getPieceAsString ('G',7) chessBoard 
                                    ++ getPieceAsString ('H',7) chessBoard 
                        ++ "\n"
                        ++ "(6)  "  ++ getPieceAsString ('A',6) chessBoard
                                    ++ getPieceAsString ('B',6) chessBoard 
                                    ++ getPieceAsString ('C',6) chessBoard 
                                    ++ getPieceAsString ('D',6) chessBoard 
                                    ++ getPieceAsString ('E',6) chessBoard 
                                    ++ getPieceAsString ('F',6) chessBoard 
                                    ++ getPieceAsString ('G',6) chessBoard 
                                    ++ getPieceAsString ('H',6) chessBoard 
                        ++ "\n"
                        ++ "(5)  "  ++ getPieceAsString ('A',5) chessBoard
                                    ++ getPieceAsString ('B',5) chessBoard 
                                    ++ getPieceAsString ('C',5) chessBoard 
                                    ++ getPieceAsString ('D',5) chessBoard 
                                    ++ getPieceAsString ('E',5) chessBoard 
                                    ++ getPieceAsString ('F',5) chessBoard 
                                    ++ getPieceAsString ('G',5) chessBoard 
                                    ++ getPieceAsString ('H',5) chessBoard 
                        ++ "\n"
                        ++ "(4)  "  ++ getPieceAsString ('A',4) chessBoard
                                    ++ getPieceAsString ('B',4) chessBoard 
                                    ++ getPieceAsString ('C',4) chessBoard 
                                    ++ getPieceAsString ('D',4) chessBoard 
                                    ++ getPieceAsString ('E',4) chessBoard 
                                    ++ getPieceAsString ('F',4) chessBoard 
                                    ++ getPieceAsString ('G',4) chessBoard 
                                    ++ getPieceAsString ('H',4) chessBoard 
                        ++ "\n"
                        ++ "(3)  "   ++ getPieceAsString ('A',3) chessBoard
                                    ++ getPieceAsString ('B',3) chessBoard 
                                    ++ getPieceAsString ('C',3) chessBoard 
                                    ++ getPieceAsString ('D',3) chessBoard 
                                    ++ getPieceAsString ('E',3) chessBoard 
                                    ++ getPieceAsString ('F',3) chessBoard 
                                    ++ getPieceAsString ('G',3) chessBoard 
                                    ++ getPieceAsString ('H',3) chessBoard 
                        ++ "\n"
                        ++ "(2)  "  ++ getPieceAsString ('A',2) chessBoard
                                    ++ getPieceAsString ('B',2) chessBoard 
                                    ++ getPieceAsString ('C',2) chessBoard 
                                    ++ getPieceAsString ('D',2) chessBoard 
                                    ++ getPieceAsString ('E',2) chessBoard 
                                    ++ getPieceAsString ('F',2) chessBoard 
                                    ++ getPieceAsString ('G',2) chessBoard 
                                    ++ getPieceAsString ('H',2) chessBoard 
                        
                        ++ "\n"
                        ++ "(1)  "  ++ getPieceAsString ('A',1) chessBoard
                                    ++ getPieceAsString ('B',1) chessBoard 
                                    ++ getPieceAsString ('C',1) chessBoard 
                                    ++ getPieceAsString ('D',1) chessBoard 
                                    ++ getPieceAsString ('E',1) chessBoard 
                                    ++ getPieceAsString ('F',1) chessBoard 
                                    ++ getPieceAsString ('G',1) chessBoard 
                                    ++ getPieceAsString ('H',1) chessBoard     
                        ++ "\n\n" 



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


-- Returns true if the chessboard has been closed to indicate the game is over
gameOver (ChessBoard _ state) = state == Over


-- Returns a version of the chessboard with only the pieces of the given colour
filterChessBoard :: ChessBoard -> ChessPieceColour -> ChessBoard
filterChessBoard (ChessBoard ogPieces state) colour = ChessBoard (filter (\ (position, piece) -> (getPieceColour piece) == colour) ogPieces) state