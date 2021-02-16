module AIPlayer where

import ChessBoard

-- Makes the AI player's move on the chess board.
-- Calculates the best next move and changes the board accordingly.
-- Returns the changed board after playing the move.
-- TODO: Implement this
aiPlayer :: ChessBoard -> IO ChessBoard
aiPlayer chessBoard = 
    do 
        putStrLn "AI's turn. Please wait."
        return chessBoard