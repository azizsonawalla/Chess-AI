module Main where

import HumanPlayer
import AIPlayer
import ChessUtilTypes
import ChessBoard

-- A chess player that has already been designated a colour/side
type ChessPlayerWithSide = ChessBoard -> IO ChessBoard

playChess :: ChessBoard -> ChessPlayerWithSide -> ChessPlayerWithSide -> IO ()
playChess chessBoard currPlayer nextPlayer = 
    do
        chessBoard <- currPlayer chessBoard                        -- Current player makes a move
        if (gameOver chessBoard)                                   -- If game is over, stop
        then do
            putStrLn "Thanks for playing! Goodbye."
            return ()
        else
            playChess chessBoard nextPlayer currPlayer             -- Continue playing. nextPplayer is now current player.
            

-- Starts a new game of chess where human player goes first (i.e. human is on white side)
main = playChess freshBoard (humanPlayer White) (aiPlayer Black)