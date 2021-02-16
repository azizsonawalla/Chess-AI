module AIPlayer where

import ChessUtilTypes

-- Makes the AI player's move on the chess board.
-- Calculates the best next move and changes the board accordingly.
-- Returns the changed board after playing the move.
-- TODO: Implement this
aiPlayer :: ChessPlayer
aiPlayer chessPieceColour chessBoard = 
    do 
        putStrLn "AI's turn. Please wait."
        return chessBoard


-- Analyzes the board and returns the best move to make
-- chessboard (ChessBoard):    the current board
-- colour (ChessPieceColour):  the colour/side of the current player 
-- TODO: Implement this. First round = randomly selected move. Second round = minmax algorithm
getBestMove :: ChessBoard -> ChessPieceColour -> ChessMove
getBestMove chessboard colour = ChessMove ('A', 0) ('A', 0)