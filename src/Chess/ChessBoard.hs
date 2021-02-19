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
chessBoardAsString chessBoard = "\n*--*--* Current Chess Board *--*--*\n\n"
                        ++ "     (A)(B)(C)(D)(E)(F)(G)(H) \n\n" 
                        ++ "(1)  "  ++ retrievePieceAt ('A',1) chessBoard
                                    ++ retrievePieceAt ('B',1) chessBoard 
                                    ++ retrievePieceAt ('C',1) chessBoard 
                                    ++ retrievePieceAt ('D',1) chessBoard 
                                    ++ retrievePieceAt ('E',1) chessBoard 
                                    ++ retrievePieceAt ('F',1) chessBoard 
                                    ++ retrievePieceAt ('G',1) chessBoard 
                                    ++ retrievePieceAt ('H',1) chessBoard 
                        ++ "\n" 
                        ++ "(2)  "  ++ retrievePieceAt ('A',2) chessBoard
                                    ++ retrievePieceAt ('B',2) chessBoard 
                                    ++ retrievePieceAt ('C',2) chessBoard 
                                    ++ retrievePieceAt ('D',2) chessBoard 
                                    ++ retrievePieceAt ('E',2) chessBoard 
                                    ++ retrievePieceAt ('F',2) chessBoard 
                                    ++ retrievePieceAt ('G',2) chessBoard 
                                    ++ retrievePieceAt ('H',2) chessBoard 
                        ++ "\n"
                        ++ "(3)  "  ++ retrievePieceAt ('A',3) chessBoard
                                    ++ retrievePieceAt ('B',3) chessBoard 
                                    ++ retrievePieceAt ('C',3) chessBoard 
                                    ++ retrievePieceAt ('D',3) chessBoard 
                                    ++ retrievePieceAt ('E',3) chessBoard 
                                    ++ retrievePieceAt ('F',3) chessBoard 
                                    ++ retrievePieceAt ('G',3) chessBoard 
                                    ++ retrievePieceAt ('H',3) chessBoard 
                        ++ "\n"
                        ++ "(4)  "  ++ retrievePieceAt ('A',4) chessBoard
                                    ++ retrievePieceAt ('B',4) chessBoard 
                                    ++ retrievePieceAt ('C',4) chessBoard 
                                    ++ retrievePieceAt ('D',4) chessBoard 
                                    ++ retrievePieceAt ('E',4) chessBoard 
                                    ++ retrievePieceAt ('F',4) chessBoard 
                                    ++ retrievePieceAt ('G',4) chessBoard 
                                    ++ retrievePieceAt ('H',4) chessBoard 
                        ++ "\n"
                        ++ "(5)  "  ++ retrievePieceAt ('A',5) chessBoard
                                    ++ retrievePieceAt ('B',5) chessBoard 
                                    ++ retrievePieceAt ('C',5) chessBoard 
                                    ++ retrievePieceAt ('D',5) chessBoard 
                                    ++ retrievePieceAt ('E',5) chessBoard 
                                    ++ retrievePieceAt ('F',5) chessBoard 
                                    ++ retrievePieceAt ('G',5) chessBoard 
                                    ++ retrievePieceAt ('H',5) chessBoard 
                        ++ "\n"
                        ++ "(6)  "  ++ retrievePieceAt ('A',6) chessBoard
                                    ++ retrievePieceAt ('B',6) chessBoard 
                                    ++ retrievePieceAt ('C',6) chessBoard 
                                    ++ retrievePieceAt ('D',6) chessBoard 
                                    ++ retrievePieceAt ('E',6) chessBoard 
                                    ++ retrievePieceAt ('F',6) chessBoard 
                                    ++ retrievePieceAt ('G',6) chessBoard 
                                    ++ retrievePieceAt ('H',6) chessBoard 
                        ++ "\n"
                        ++ "(7)  "  ++ retrievePieceAt ('A',7) chessBoard
                                    ++ retrievePieceAt ('B',7) chessBoard 
                                    ++ retrievePieceAt ('C',7) chessBoard 
                                    ++ retrievePieceAt ('D',7) chessBoard 
                                    ++ retrievePieceAt ('E',7) chessBoard 
                                    ++ retrievePieceAt ('F',7) chessBoard 
                                    ++ retrievePieceAt ('G',7) chessBoard 
                                    ++ retrievePieceAt ('H',7) chessBoard 
                        ++ "\n"
                        ++ "(8)  "  ++ retrievePieceAt ('A',8) chessBoard
                                    ++ retrievePieceAt ('B',8) chessBoard 
                                    ++ retrievePieceAt ('C',8) chessBoard 
                                    ++ retrievePieceAt ('D',8) chessBoard 
                                    ++ retrievePieceAt ('E',8) chessBoard 
                                    ++ retrievePieceAt ('F',8) chessBoard 
                                    ++ retrievePieceAt ('G',8) chessBoard 
                                    ++ retrievePieceAt ('H',8) chessBoard 
                        ++ "\n\n" 

retrievePieceAt:: ChessPosition -> ChessBoard -> [Char]
retrievePieceAt (char,num) board
    | getPieceAt (char,num) board == Nothing                    = empty
    | getPieceAt (char,num) board == Just (King White)          = whiteKing
    | getPieceAt (char,num) board == Just (Queen White)         = whiteQueen
    | getPieceAt (char,num) board == Just (Rook White)          = whiteRook
    | getPieceAt (char,num) board == Just (Bishop White)        = whiteBishop
    | getPieceAt (char,num) board == Just (Knight White)        = whiteKnight
    | getPieceAt (char,num) board == Just (Pawn White)          = whitePawn
    | getPieceAt (char,num) board == Just (King Black)          = blackKing
    | getPieceAt (char,num) board == Just (Queen Black)         = blackQueen
    | getPieceAt (char,num) board == Just (Rook Black)          = blackRook
    | getPieceAt (char,num) board == Just (Bishop Black)        = blackBishop
    | getPieceAt (char,num) board == Just (Knight Black)        = blackKnight
    | getPieceAt (char,num) board == Just (Pawn Black)          = blackPawn
    where
            empty = "[ ]"
            whiteKing = "[\x2654]"
            whiteQueen = "[\x2655]"
            whiteRook = "[\x2656]"
            whiteBishop = "[\x2657]"
            whiteKnight = "[\x2658]"
            whitePawn = "[\x2659]"

            blackKing = "[\x265A]"
            blackQueen = "[\x265B]"
            blackRook = "[\x265C]"
            blackBishop = "[\x265D]"
            blackKnight = "[\x265E]"
            blackPawn = "[\x265F]"


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