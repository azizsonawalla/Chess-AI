module ChessBoard where

import ChessPieces
import ChessUtilTypes
import FENotation
import Debug.Trace


-- -- Define Show for a ChessBoard
-- instance Show ChessBoard where
--     show board = chessBoardAsString board


-- A fresh Chess Board with all the pieces in the starting position
freshBoard = fenToChessBoard "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR"


-- Returns the string representation of a chessboard
-- TODO: implement + test this (2 hours). [Cynthia]
-- Note: one way to implement this is to have a 'template' string (possibly stored in a txt file) and then simply
--       replace a placeholder in each square with the piece that is currently there.
testboard = (ChessBoard [ (('A', 1), Rook White) ] Ongoing)

chessBoardAsString :: ChessBoard -> IO ()
chessBoardAsString (ChessBoard [(position, piece)] _) = 
    do
        putStr board
        where
            empty = "[  ]"
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

        --  process and print board
            board   =    "\n*--*--* Current Chess Board *--*--*\n\n"
                        ++ "     (1)(2)(3)(4)(5)(6)(7)(8) \n\n" 
                        ++ "(A)  " ++ concat [whitePawn | i <- [1..8]] ++ "\n" 
                        ++ "(B)  " ++ concat [whitePawn | i <- [1..8]] ++ "\n"
                        ++ "(C)  " ++ concat [whitePawn | i <- [1..8]] ++ "\n"
                        ++ "(D)  " ++ concat [whitePawn | i <- [1..8]] ++ "\n"
                        ++ "(E)  " ++ concat [whitePawn | i <- [1..8]] ++ "\n"
                        ++ "(F)  " ++ concat [whitePawn | i <- [1..8]] ++ "\n"
                        ++ "(G)  " ++ concat [whitePawn | i <- [1..8]] ++ "\n"
                        ++ "(H)  " ++ concat [whitePawn | i <- [1..8]] ++ "\n" 
                        ++ "\n"
                        

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