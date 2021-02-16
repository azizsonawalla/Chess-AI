module HumanPlayer where

import ChessUtilTypes

-- Facilitates the human player's move on the chess board.
-- Shows the human player the board, asks for next move, and changes the board accordingly.
-- Returns changed board after human player's move
-- TODO: Implement this
humanPlayer :: ChessPlayer
humanPlayer chessPieceColour chessBoard = 
    do 
        putStrLn "Your turn:"
        return chessBoard