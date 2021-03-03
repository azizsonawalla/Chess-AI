module ChessBoard where

import ChessPieces
import ChessUtilTypes
import FENotation
import Data.Maybe
import Util


-- -- Define Show for a ChessBoard
instance Show ChessBoard where
    show board = (chessBoardAsString board)

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
makeMove :: ChessBoard -> ChessMove -> ChessBoard
makeMove cb@(ChessBoard pieces state) (ChessMove from to) = updateGameStatus newChessBoard
    where newChessBoard = ChessBoard (putPiece pieceToMove to (removePiece from pieces)) state
          pieceToMove = fromJust(getPieceAt from cb)


-- Checks if there is a checkmate and updates the game status accordingly
-- TODO: test this
updateGameStatus :: ChessBoard -> ChessBoard
updateGameStatus chessBoard@(ChessBoard pieces state) = (ChessBoard pieces newState)
    where newState = if (whitekingDead || blackkingDead) then Over else state
          whitekingDead = kingDead chessBoard White
          blackkingDead = kingDead chessBoard Black


-- Check if the given King is in checkmate
kingDead :: ChessBoard -> ChessPieceColour -> Bool
kingDead chessBoard colour = (getPositionOfPiece chessBoard (King colour)) == Nothing


-- Returns the position of the given piece on the board (if multiple, returns last)
getPositionOfPiece :: ChessBoard -> ChessPiece -> Maybe ChessPosition
getPositionOfPiece (ChessBoard pieces _) piece = foldl (\ result (position, currPiece) -> if currPiece == piece then Just position else result) Nothing pieces


-- Removes the piece at the given position from the ChessBoard if it exists
-- Does nothing if the position is empty
removePiece :: ChessPosition -> [(ChessPosition, ChessPiece)] -> [(ChessPosition, ChessPiece)]
removePiece chessPosition board = filter (\ (position, piece) -> position /= chessPosition) board


-- Adds the given piece to the given position on the chessboard
-- If there is a piece at the given position, then that piece will be removed
putPiece :: ChessPiece -> ChessPosition -> [(ChessPosition, ChessPiece)] -> [(ChessPosition, ChessPiece)]
putPiece chessPiece chessPosition board = (removePiece chessPosition board) ++ [(chessPosition, chessPiece)]


-- Returns all legal moves for the given side on the given chess board
legalMoves :: ChessBoard -> ChessPieceColour -> [ChessMove]
legalMoves chessBoard chessPieceColour = foldr (\ (position, piece) allMoves -> allMoves ++ (legalMovesForPieceAtPos piece chessBoard position)) [] filteredPieces
    where (ChessBoard filteredPieces state) = filterChessBoard chessBoard chessPieceColour


-- Returns true if the given move is valid on the given board, for the given colour
validMove :: ChessBoard -> ChessPieceColour -> ChessMove -> Bool
validMove board colour move = elem move (legalMoves board colour)


-- Returns true if the chessboard has been closed to indicate the game is over
gameOver (ChessBoard _ state) = state == Over


-- Returns the pieces on the ChessBoard
getPieces (ChessBoard pieces _) = pieces


-- Returns a version of the chessboard with only the pieces of the given colour
filterChessBoard :: ChessBoard -> ChessPieceColour -> ChessBoard
filterChessBoard (ChessBoard ogPieces state) colour = ChessBoard (filter (\ (position, piece) -> (getPieceColour piece) == colour) ogPieces) state