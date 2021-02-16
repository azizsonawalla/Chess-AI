module ChessBoard where

-- A Chess Board
-- TODO: Implement this
data ChessBoard = Null 

-- Define Show for a ChessBoard
instance Show ChessBoard where
    show board = chessBoardAsString board

-- Returns the string representation of a chessboard
-- TODO: implement this
chessBoardAsString Null = "A null chessboard"
chessBoardAsString board = "Not implemented"

-- Returns true if the chessboard has been closed to indicate the game is over
-- TODO: Implement this
gameOver chessboard = False

-- A fresh Chess Board with all the pieces in the starting position
-- TODO: Implement this
freshBoard = Null