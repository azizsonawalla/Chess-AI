module ChessPieces where

import ChessUtilTypes
import FENotation
import Data.List
import Data.Maybe


-- Returns all the legal moves for the given piece, at the given position, on the given chess board
-- Warning: Assumes that the given ChessPiece is at the given ChessPosition
-- TODO: test
legalMovesForPieceAtPos :: ChessPiece -> ChessBoard -> ChessPosition -> [ChessMove]
legalMovesForPieceAtPos piece board startPosition = buildMoves startPosition (legalNextPosForPieceAtPos piece board startPosition)


-- Returns the list of positions that the given piece, at the given position, can move to on the chess board
-- Warning: Assumes that the given ChessPiece is at the given ChessPosition
legalNextPosForPieceAtPos :: ChessPiece -> ChessBoard -> ChessPosition -> [ChessPosition]

-- King can move exactly one square horizontally, vertically, or diagonally (ignore castling for now).
-- King cannot move to a square if a piece of its own colour is blocking the way.
-- TODO: Implement + test this (moves for King at the given position) (1.5 hours) [Cynthia]
legalNextPosForPieceAtPos (King colour) chessBoard position = []

-- Queen can move any number of vacant squares in any direction
-- Queen cannot move to a square if a piece of its own colour is blocking the way (cannot jump)
-- If a piece of another colour is blocking the way, the Queen must stop at that square (i.e. kill the piece, cannot jump)
-- TODO: Implement + test this (moves for Queen at the given position) (1 hour) [Yiyi]
-- Note: Notice that the Queen combines the moves of the Rook and Bishop. An easy way to implement this is to:
--     1. Replace the queen with a rook of the same colour and call legalNextPosForPieceAtPos for the rook
--     2. Replace the queen with a bishop of the same colour and call legalNextPosForPieceAtPos for the bishop
--     3. Return a concatenation of the moves calculated in 1 and 2
legalNextPosForPieceAtPos (Queen colour) chessBoard position = []

-- Rook can move any number of vacant squares vertically or horizontally (ignore castlling for now).
-- Rook cannot move to a square if a piece of its own colour is blocking the way (cannot jump)
-- If a piece of another colour is blocking the way, the Rook must stop at that square (i.e. kill the piece, cannot jump)
-- TODO: Implement + test this (moves for Rook at the given position) (2 hour) [Aziz]
legalNextPosForPieceAtPos (Rook colour) chessBoard position = []

-- Bishop can move any number of vacant squares in any diagonal direction.
-- Bishop cannot move to a square if a piece of its own colour is blocking the way (cannot jump)
-- If a piece of another colour is blocking the way, the Bishop must stop at that square (i.e. kill the piece, cannot jump)
-- TODO: Implement + test this (moves for Bishop at the given position) (2 hour) [Cynthia]
legalNextPosForPieceAtPos (Bishop colour) chessBoard position = []

-- Knight can move in an “L” laid out at any horizontal or vertical angle. That is, two squares in any straight line 
-- and then one at a right-angle. The knight can also jump over pieces. 
-- TODO: Implement + test this (moves for Knight at the given position) (1.5 hour) [Yiyi]
legalNextPosForPieceAtPos (Knight colour) chessBoard position = []

-- The pawn moves by the following rules:
--     - It can only move forwards
--     - It can move one square straight forward if there is no piece ahead of it (it cannot kill a piece moving straight forward)
--     - It can move diagonally forward (left or right) only if there is a piece of the other colour in the destination square
--     - If the pawn is moving for the first time, it can move 2-squares forward if there is nothing blocking it. 
--       It may also capture/kill a piece at the destination square this way.
-- TODO: Implement + test this (moves for Pawn at the given position) (1.5 hour) [Aziz]
legalNextPosForPieceAtPos (Pawn White) chessBoard (col, 2) = up1 ++ up2 ++ topLeft ++ topRight  -- start position white pawn
                                                             where up1 = if up1SquareEmpty then [(col, 3)] else []
                                                                   up2 = if up2SquaresEmpty then [(col, 4)] else []
                                                                   topLeft = if topLeftHasBlackPiece then [getTopLeftPos (col, 2)] else []
                                                                   topRight = if topRightHasBlackPiece then [getTopRightPos (col, 2)] else []
                                                                   up2SquaresEmpty = up1SquareEmpty && (isEmpty (col, 4) chessBoard)
                                                                   up1SquareEmpty = isEmpty (col, 3) chessBoard
                                                                   topLeftHasBlackPiece = (col /= 'A') && (not (isEmpty (getTopLeftPos (col, 2)) chessBoard)) && ((getColourOfPieceAt (getTopLeftPos (col, 2)) chessBoard) == (Just Black))
                                                                   topRightHasBlackPiece = (col /= 'H') && (not (isEmpty (getTopRightPos (col, 2)) chessBoard)) && ((getColourOfPieceAt (getTopRightPos (col, 2)) chessBoard) == (Just Black))
legalNextPosForPieceAtPos (Pawn White) chessBoard (col, row) = [] -- non-start position white pawn
legalNextPosForPieceAtPos (Pawn Black) chessBoard (col, 7) = down1 ++ down2 ++ bottomLeft ++ bottomRight  -- start position black pawn
                                                             where down1 = if bottom1SquareEmpty then [(col, 6)] else []
                                                                   down2 = if bottom2SquaresEmpty then [(col, 5)] else []
                                                                   bottomLeft = if bottomLeftHasWhitePiece then [getBottomLeftPos (col, 7)] else []
                                                                   bottomRight = if bottomRightHasWhitePiece then [getBottomRightPos (col, 7)] else []
                                                                   bottom2SquaresEmpty = bottom1SquareEmpty && (isEmpty (col, 5) chessBoard)
                                                                   bottom1SquareEmpty = isEmpty (col, 6) chessBoard
                                                                   bottomLeftHasWhitePiece = (col /= 'A') && (not (isEmpty (getBottomLeftPos (col, 7)) chessBoard)) && ((getColourOfPieceAt (getBottomLeftPos (col, 7)) chessBoard) == (Just White))
                                                                   bottomRightHasWhitePiece = (col /= 'H') && (not (isEmpty (getBottomRightPos (col, 7)) chessBoard)) && ((getColourOfPieceAt (getBottomRightPos (col, 7)) chessBoard) == (Just White))
legalNextPosForPieceAtPos (Pawn Black) chessBoard (col, row) = [] -- non-start position black pawn


-- Returns a list of ChessMoves from the given start position to all the destination positions
buildMoves :: ChessPosition -> [ChessPosition] -> [ChessMove]
buildMoves startPosition endPositions = map (\ endPosition -> ChessMove startPosition endPosition) endPositions


-- Returns the chess piece at the given position as a Maybe
getPieceAt :: ChessPosition -> ChessBoard -> Maybe ChessPiece
getPieceAt position (ChessBoard pieces _) = lookup position pieces


-- Returns the chess piece at the given position as a Maybe
-- TODO: test this
getColourOfPieceAt :: ChessPosition -> ChessBoard -> Maybe ChessPieceColour
getColourOfPieceAt position chessBoard = if piece /= Nothing then Just (getPieceColour (fromJust piece)) else Nothing where piece = getPieceAt position chessBoard 


-- Returns true if there is no piece on the board at the given position
isEmpty :: ChessPosition -> ChessBoard -> Bool
isEmpty position board = (getPieceAt position board) == Nothing


-- Returns the position at the top-left diagonal of the given position
-- Warning: given position should not be in column 'A' or in row 8
-- TODO: test this
getTopLeftPos :: ChessPosition -> ChessPosition
getTopLeftPos (col, row) = (leftCol, row+1) where leftCol = chessBoardCols !! ((fromJust (elemIndex col chessBoardCols)) - 1)


-- Returns the position at the top-right diagonal of the given position
-- Warning: given position should not be in column 'H' or in row 8
-- TODO: test this
getTopRightPos :: ChessPosition -> ChessPosition
getTopRightPos (col, row) = (rightCol, row+1) where rightCol = chessBoardCols !! ((fromJust (elemIndex col chessBoardCols)) + 1)


-- Returns the position at the bottom-left diagonal of the given position
-- Warning: given position should not be in column 'A' or in row 1
-- TODO: test this
getBottomLeftPos :: ChessPosition -> ChessPosition
getBottomLeftPos (col, row) = (leftCol, row-1) where leftCol = chessBoardCols !! ((fromJust (elemIndex col chessBoardCols)) - 1)


-- Returns the position at the bottom-right diagonal of the given position
-- Warning: given position should not be in column 'H' or in row 1
-- TODO: test this
getBottomRightPos :: ChessPosition -> ChessPosition
getBottomRightPos (col, row) = (rightCol, row-1) where rightCol = chessBoardCols !! ((fromJust (elemIndex col chessBoardCols)) + 1)


-- Returns the colour of the given ChessPiece
getPieceColour :: ChessPiece -> ChessPieceColour
getPieceColour (Pawn colour) = colour
getPieceColour (King colour) = colour
getPieceColour (Queen colour) = colour
getPieceColour (Rook colour) = colour
getPieceColour (Bishop colour) = colour
getPieceColour (Knight colour) = colour