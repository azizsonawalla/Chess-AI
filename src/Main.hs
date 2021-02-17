module Main where

import HumanPlayer
import AIPlayer
import ChessUtilTypes
import ChessBoard

-- A chess player that has already been designated a colour/side
type ChessPlayerWithSide = ChessBoard -> IO ChessBoard

welcomeMessage = "\n===================\nWelcome to Chess-AI\n===================\n"

playChess :: ChessBoard -> ChessPlayer -> ChessPlayer-> IO ()
playChess chessBoard firstPlayer secondPlayer = 
    do
        putStrLn welcomeMessage
        handleNextTurn chessBoard (firstPlayer White) (secondPlayer Black)   -- first player is always White


handleNextTurn :: ChessBoard -> ChessPlayerWithSide -> ChessPlayerWithSide -> IO ()           
handleNextTurn chessBoard currPlayer nextPlayer  = 
    do
        chessBoard <- currPlayer chessBoard                        -- Current player makes a move
        if (gameOver chessBoard)                                   -- If game is over, stop
        then do
            putStrLn "Thanks for playing! Goodbye."
            return ()
        else
            handleNextTurn chessBoard nextPlayer currPlayer             -- Continue playing. nextPplayer is now current player. 


-- Starts a new game of chess where human player goes first (i.e. human is on white side)
main = playChess freshBoard humanPlayer aiPlayer