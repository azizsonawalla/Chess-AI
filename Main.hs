module Main where

import HumanPlayer
import ChessBoard
import AIPlayer

type ChessPlayer = (ChessBoard -> IO ChessBoard)

playChess :: ChessBoard -> ChessPlayer -> ChessPlayer -> IO ()
playChess chessBoard currPlayer nextPlayer = 
    do
        chessBoard <- currPlayer chessBoard                        -- Current player makes a move
        if (gameOver chessBoard)                                   -- If game is over, stop
        then do
            putStrLn "Thanks for playing! Goodbye."
            return ()
        else
            playChess chessBoard nextPlayer currPlayer             -- Continue playing. nextPplayer is now current player.
            

-- Starts a new game of chess where human player goes first
main = playChess freshBoard humanPlayer aiPlayer