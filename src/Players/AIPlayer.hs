module AIPlayer where

import ChessUtilTypes
import ChessBoard

-- Makes the AI player's move on the chess board.
-- Calculates the best next move and changes the board accordingly.
-- Returns the changed board after playing the move.
aiPlayer :: ChessPlayer
aiPlayer chessPieceColour chessBoard = 
    do 
        putStrLn "AI's turn. Please wait."
        let move = getBestMove chessBoard chessPieceColour
        let newChessBoard = makeMove chessBoard move
        return newChessBoard


-- Analyzes the board and returns the best move to make
-- chessboard (ChessBoard):    the current board
-- colour (ChessPieceColour):  the colour/side of the current player 
-- TODO: Implement + test this. First round = randomly selected move (1 hour). Second round = minmax algorithm
getBestMove :: ChessBoard -> ChessPieceColour -> ChessMove
getBestMove chessboard colour = ChessMove ('A', 0) ('A', 0)