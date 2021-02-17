module HumanPlayer where

import ChessUtilTypes
import ChessBoard


-- Facilitates the human player's move on the chess board.
-- Shows the human player the board, asks for next move, and changes the board accordingly.
-- Returns changed board after human player's move
humanPlayer :: ChessPlayer
humanPlayer chessPieceColour chessBoard = 
    do 
        putStrLn "Your turn:"
        putStrLn (show chessBoard)                                                  -- show the user the chess board
        putStrLn "Enter a move (eg. c1 to d2): "                                    -- prompt for move
        moveStr <- getLine                                                          -- get move entered by user
        if (validMoveString moveStr)                                                -- check if string is in valid format
        then (handleValidInput moveStr chessPieceColour chessBoard)
        else (handleInvalidInput moveStr chessPieceColour chessBoard)


-- Handles the case where the human player enters an input with invalid format
handleInvalidInput :: String -> ChessPieceColour -> ChessBoard -> IO ChessBoard
handleInvalidInput moveStr chessPieceColour chessBoard =
    do
        putStrLn ("'"++moveStr++"' is an invalid format. Try again.")               -- ask human to try again
        (humanPlayer chessPieceColour chessBoard)                                   -- retry human player protocol


-- Handles the case where the human player enters an input with a valid format
handleValidInput :: String -> ChessPieceColour -> ChessBoard -> IO ChessBoard
handleValidInput moveStr chessPieceColour chessBoard = 
    do
        let move = stringToChessMove moveStr                                        -- convert string to ChessMove
        if (validMove chessBoard chessPieceColour move)                             -- check if the move itself is valid given the current board and the player's colour
        then do
            return (makeMove chessBoard move)                                       -- return board after making move
        else do
            putStrLn ("'"++moveStr++"' is an invalid move. Try again.")             -- ask human to try again
            (humanPlayer chessPieceColour chessBoard)                               -- recursive call to player


-- Checks if the given string has a valid format for a chess move
-- Valid format = [a-h][1-8] to [a-h][1-8]
-- TODO: implement + test this (1 hour)
validMoveString :: String -> Bool
validMoveString str = False


-- Converts a chess move from the "[a-h][1-8] to [a-h][1-8]" format to a ChessMove
-- WARNING: Assumes that the string is in a valid format
-- TODO: Implement + test this (1.5 hour)
stringToChessMove :: String -> ChessMove
stringToChessMove _ = ChessMove ('A', 0) ('A', 0)