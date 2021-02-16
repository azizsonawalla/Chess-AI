module HumanPlayer where

import ChessBoard

-- Facilitates the human player's move on the chess board.
-- Shows the human player the board, asks for next move, and changes the board accordingly.
-- Returns changed board after human player's move
-- TODO: Implement this
humanTurn :: ChessBoard -> IO ChessBoard
humanTurn chessBoard = 
    do 
        putStrLn "Your turn:"
        return chessBoard