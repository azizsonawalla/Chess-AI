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
makeMove cb@(ChessBoard pieces state) (ChessMove from to)
    | (isEmpty to cb) =
        ChessBoard ((filter (\ (position, piece) -> piece /= pieceFrom) pieces) ++ [(to, pieceFrom)]) newState
    | otherwise =
        ChessBoard ((filter (\ (position, piece) -> piece /= pieceFrom && piece /= fromJust (getPieceAt to cb)) pieces) ++ [(to, pieceFrom)]) newState 
    where pieceFrom = fromJust (getPieceAt from cb)
          newState = if (isCheckMate cb (getPieceColour pieceFrom)) then Over else state

        

isCheckMate :: ChessBoard -> ChessPieceColour -> Bool
isCheckMate checkBoard colour = False

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