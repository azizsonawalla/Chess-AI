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
legalNextPosForPieceAtPos (Pawn White) chessBoard (col, row) = moveUp1 ++ (if row == 2 then moveUp2 else []) ++ moveLeft ++ moveRight
                                                             where moveUp1 = if top1Empty && (row < 8) then [(col, row+1)] else []
                                                                   moveUp2 = if top2Empty && (row < 7) then [(col, row+2)] else []
                                                                   moveLeft = if topLeftHasBlackPiece then [getTopLeftPos (col, row)] else []
                                                                   moveRight = if topRightHasBlackPiece then [getTopRightPos (col, row)] else []
                                                                   top2Empty = top1Empty && (isEmpty (col, row+2) chessBoard)
                                                                   top1Empty = isEmpty (col, row+1) chessBoard
                                                                   topLeftHasBlackPiece = (col /= 'A') && (row /= 8) && (not (isEmpty topLeft chessBoard)) && ((getColourOfPieceAt topLeft chessBoard) == (Just Black))
                                                                   topRightHasBlackPiece = (col /= 'H') && (row /= 8) && (not (isEmpty topRight chessBoard)) && ((getColourOfPieceAt topRight chessBoard) == (Just Black))
                                                                   topLeft = getTopLeftPos (col, row)
                                                                   topRight = getTopRightPos (col, row)
legalNextPosForPieceAtPos (Pawn Black) chessBoard (col, row) = down1 ++ (if row == 7 then down2 else []) ++ bottomLeft ++ bottomRight
                                                             where down1 = if bottom1SquareEmpty && (row > 1) then [(col, row-1)] else []
                                                                   down2 = if bottom2SquaresEmpty && (row > 2) then [(col, row-2)] else []
                                                                   bottomLeft = if bottomLeftHasWhitePiece then [getBottomLeftPos (col, row)] else []
                                                                   bottomRight = if bottomRightHasWhitePiece then [getBottomRightPos (col, row)] else []
                                                                   bottom2SquaresEmpty = bottom1SquareEmpty && (isEmpty (col, row-2) chessBoard)
                                                                   bottom1SquareEmpty = isEmpty (col, row-1) chessBoard
                                                                   bottomLeftHasWhitePiece = (col /= 'A') && (row /= 1) && (not (isEmpty (getBottomLeftPos (col, row)) chessBoard)) && ((getColourOfPieceAt (getBottomLeftPos (col, row)) chessBoard) == (Just White))
                                                                   bottomRightHasWhitePiece = (col /= 'H') && (row /= 1) && (not (isEmpty (getBottomRightPos (col, row)) chessBoard)) && ((getColourOfPieceAt (getBottomRightPos (col, row)) chessBoard) == (Just White))


-- Returns a list of ChessMoves from the given start position to all the destination positions
buildMoves :: ChessPosition -> [ChessPosition] -> [ChessMove]
buildMoves startPosition endPositions = map (\ endPosition -> ChessMove startPosition endPosition) endPositions


-- Returns the chess piece at the given position as a Maybe
getPieceAt :: ChessPosition -> ChessBoard -> Maybe ChessPiece
getPieceAt position (ChessBoard pieces _) = lookup position pieces


-- Returns the chess piece at the given position as a Maybe
getColourOfPieceAt :: ChessPosition -> ChessBoard -> Maybe ChessPieceColour
getColourOfPieceAt position chessBoard = if piece /= Nothing then Just (getPieceColour (fromJust piece)) else Nothing where piece = getPieceAt position chessBoard 


-- Returns true if there is no piece on the board at the given position
isEmpty :: ChessPosition -> ChessBoard -> Bool
isEmpty position board = (getPieceAt position board) == Nothing


-- Returns the position at the top-left diagonal of the given position
-- Warning: given position should not be in column 'A' or in row 8
getTopLeftPos :: ChessPosition -> ChessPosition
getTopLeftPos (col, row) = (leftCol, row+1) where leftCol = chessBoardCols !! ((fromJust (elemIndex col chessBoardCols)) - 1)


-- Returns the position at the top-right diagonal of the given position
-- Warning: given position should not be in column 'H' or in row 8
getTopRightPos :: ChessPosition -> ChessPosition
getTopRightPos (col, row) = (rightCol, row+1) where rightCol = chessBoardCols !! ((fromJust (elemIndex col chessBoardCols)) + 1)


-- Returns the position at the bottom-left diagonal of the given position
-- Warning: given position should not be in column 'A' or in row 1
getBottomLeftPos :: ChessPosition -> ChessPosition
getBottomLeftPos (col, row) = (leftCol, row-1) where leftCol = chessBoardCols !! ((fromJust (elemIndex col chessBoardCols)) - 1)


-- Returns the position at the bottom-right diagonal of the given position
-- Warning: given position should not be in column 'H' or in row 1
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


chessPieceStrings = [((King   White), "[ K ]")
                    ,((Queen  White), "[ Q ]")
                    ,((Rook   White), "[ R ]")
                    ,((Bishop White), "[ B ]")
                    ,((Knight White), "[ N ]")
                    ,((Pawn   White), "[ P ]")
                    ,((King   Black), "[ k ]")
                    ,((Queen  Black), "[ q ]")
                    ,((Rook   Black), "[ r ]")
                    ,((Bishop Black), "[ b ]")
                    ,((Knight Black), "[ n ]")
                    ,((Pawn   Black), "[ p ]")]
getPieceAsString:: ChessPosition -> ChessBoard -> [Char]
getPieceAsString position board = if piece /= Nothing then fromJust (lookup (fromJust piece) chessPieceStrings) else "[   ]"  where piece = getPieceAt position board
