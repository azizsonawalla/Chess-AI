module ChessBoard where

import ChessPieces
import ChessUtilTypes
import FENotation
import Data.Maybe
import Util


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
-- TODO: test this (1.5 hours) [Yiyi]
makeMove :: ChessBoard -> ChessMove -> ChessBoard
makeMove chessBoard (ChessMove source dest) = updateGameStatus (putPiece cleanBoard dest pieceToMove)
    where cleanBoard = removePiece (removePiece chessBoard dest) source
          pieceToMove = fromJust (getPieceAt source chessBoard)


-- Checks if there is a checkmate and updates the game status accordingly
-- TODO: test this
updateGameStatus chessBoard@(ChessBoard pieces state) = (ChessBoard pieces newState)
    where newState = if whiteKingInCheckmate || blackKingInCheckmate then Over else state
          whiteKingInCheckmate = kingInCheckmate chessBoard White
          blackKingInCheckmate = kingInCheckmate chessBoard Black


-- Check if the given King is in checkmate
-- TODO: test this
kingInCheckmate chessBoard@(ChessBoard pieces state) colour = not (kingExists && kingSafe)
    where kingExists = (kingPos /= Nothing)
          kingSafe = not (subset kingMoves opponentMoves)
          kingPos = getPositionOfPiece chessBoard (King colour) 
          kingMoves = legalMovesForPieceAtPos (King colour) chessBoard (fromJust kingPos)
          opponentMoves = legalMoves chessBoard (oppositeColour colour)


-- Returns the position of the given piece on the board (if multiple, returns last)
getPositionOfPiece (ChessBoard pieces _) piece = foldl (\ result (position, currPiece) -> if currPiece == piece then Just position else result) Nothing pieces


-- Adds the given piece at the given position
-- Warning: if there is already a piece at the position, then there will be a duplicate entry
-- TODO: test this
putPiece (ChessBoard pieces state) position piece = ChessBoard ((position, piece):pieces) state


-- Removes the piece at a given position. Does nothing if there is no piece.
-- TODO: test this
removePiece (ChessBoard pieces state) position = (ChessBoard newPieces state)
    where newPieces = filter (\ (currPosition, piece) -> currPosition /= position) pieces


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