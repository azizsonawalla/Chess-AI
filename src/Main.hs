module Main where

import HumanPlayer
import AIPlayer
import ChessUtilTypes
import ChessBoard

welcomeMessage = "\n===================\nWelcome to Chess-AI\n===================\n\n\n"

startGame :: IO ()
startGame = 
    do
        putStr welcomeMessage
        putStrLn "What is your name?"
        humanName <- getLine
        putStrLn ("\nHi "++humanName++"!\n")
        let humanPlayer = ChessPlayer humanName White humanMoveFunction -- human player is always white
        let aiPlayer = ChessPlayer "AI Player" Black aiMoveFunction
        putStrLn "\nStarting game..\n\n"
        handleNextTurn freshBoard humanPlayer aiPlayer  -- white player always goes first


handleNextTurn :: ChessBoard -> ChessPlayer -> ChessPlayer -> IO ()           
handleNextTurn chessBoard currPlayer@(ChessPlayer name colour moveFn) nextPlayer  = 
    do
        putStrLn (name ++ "\'s turn: ("++(show colour)++")")
        (chessBoard, move) <- moveFn colour chessBoard                  -- Current player makes a move
        putStrLn (name++" played "++(show move))
        if (gameOver chessBoard)                                        -- If game is over, stop
        then do
            putStrLn (name++" wins!")
            putStrLn "Thanks for playing! Goodbye."
            return ()
        else
            handleNextTurn chessBoard nextPlayer currPlayer             -- Continue playing. nextPlayer is now current player. 


-- Starts a new game of chess where human player goes first (i.e. human is on white side)
main = startGame