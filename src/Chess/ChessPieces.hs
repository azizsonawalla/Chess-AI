module ChessPieces where

import ChessUtilTypes


-- Returns all the legal moves for the given piece, at the given position, on the given chess board
legalMovesForPieceAtPos :: ChessPiece -> ChessBoard -> ChessPosition -> [ChessMove]
legalMovesForPieceAtPos piece board startPosition = buildMoves startPosition (legalNextPosForPieceAtPos piece board startPosition)


-- Returns the list of positions that the given piece, at the given position, can move to on the chess board
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
legalNextPosForPieceAtPos (Pawn colour firstMove) chessBoard position = []


-- Returns a list of ChessMoves from the given start position to all the destination positions
buildMoves :: ChessPosition -> [ChessPosition] -> [ChessMove]
buildMoves startPosition endPositions = map (\ endPosition -> ChessMove startPosition endPosition) endPositions