module ChessPieces where

import ChessUtilTypes


-- Returns all the legal moves for the given piece, at the given position, on the given chess board
legalMovesForPieceAtPos :: ChessPiece -> ChessBoard -> ChessPosition -> [ChessMove]

-- TODO: Implement this (moves for King at the given position)
legalMovesForPieceAtPos (King colour) chessBoard position = []

-- TODO: Implement this (moves for Queen at the given position)
legalMovesForPieceAtPos (Queen colour) chessBoard position = []

-- TODO: Implement this (moves for Rook at the given position)
legalMovesForPieceAtPos (Rook colour) chessBoard position = []

-- TODO: Implement this (moves for Bishop at the given position)
legalMovesForPieceAtPos (Bishop colour) chessBoard position = []

-- TODO: Implement this (moves for Pawn at the given position)
legalMovesForPieceAtPos (Pawn colour) chessBoard position = []