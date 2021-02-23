module HumanPlayer where

import ChessUtilTypes
import ChessBoard
import Data.Char
import Util
import ChessPieces
import Data.Maybe


-- Facilitates the human player's move on the chess board.
-- Shows the human player the board, asks for next move, and changes the board accordingly.
-- Returns changed board after human player's move
humanMoveFunction :: MoveFunction
humanMoveFunction chessPieceColour chessBoard = 
    do 
        putStrLn "\n-------------------------------------------------------------------\n"
        putStrLn (show chessBoard)                                                  -- show the user the chess board
        putStrLn "Enter a move (eg. c1 to d2) or enter a position (eg. h5) to see available moves: "
        input <- getLine                                                          -- get move entered by user
        if (validMoveString input)                                                -- check if string is in valid format
        then (handleValidMoveInput input chessPieceColour chessBoard)
        else (if validPositionString input 
              then (handleValidPositionInput input chessPieceColour chessBoard)
              else (handleInvalidInput input chessPieceColour chessBoard))


-- Handles the case where the human player enters an input with invalid format
handleInvalidInput :: String -> MoveFunction
handleInvalidInput moveStr chessPieceColour chessBoard =
    do
        putStrLn ("'"++moveStr++"' is an invalid input. Try again.")               -- ask human to try again
        (humanMoveFunction chessPieceColour chessBoard)                            -- retry human player protocol


-- Handles the case where the human player enters an input with a valid move
handleValidMoveInput :: String -> MoveFunction
handleValidMoveInput moveStr chessPieceColour chessBoard = 
    do
        let move = stringToChessMove moveStr                                        -- convert string to ChessMove
        if (validMove chessBoard chessPieceColour move)                             -- check if the move itself is valid given the current board and the player's colour
        then do
            return ((makeMove chessBoard move), move)                               -- return board after making move
        else do
            putStrLn ("'"++moveStr++"' is an invalid move. Try again.")             -- ask human to try again
            (humanMoveFunction chessPieceColour chessBoard)                               -- recursive call to player


-- Handles the case where the human player enters an input with a valid position
handleValidPositionInput :: String -> MoveFunction
handleValidPositionInput posStr chessPieceColour chessBoard = 
    do
        let position = chessPosFromStr posStr
        let piece = getPieceAt position chessBoard
        if piece == Nothing
        then do
            putStrLn ("There's no piece at "++posStr)
            (humanMoveFunction chessPieceColour chessBoard)
        else do
            let legalMovesFromPos = legalMovesForPieceAtPos (fromJust piece) chessBoard position
            putStrLn ("\nYou can play: "++ (show legalMovesFromPos))
            (humanMoveFunction chessPieceColour chessBoard)


-- Checks if the given string has a valid format for a chess move
-- Valid format = [a-h][1-8] to [a-h][1-8]
validMoveString :: String -> Bool
validMoveString str = ((length parts) == 3) && (validPositionString (parts !! 0)) && ((parts !! 1) == "to") && (validPositionString (parts !! 2))
    where parts = split ' ' str


-- Returns true if the given string represents a valid chess position
validPositionString :: String -> Bool 
validPositionString str = ((length str) == 2) && (elem firstChar "ABCDEFGH") && (elem secondChar "12345678")
    where firstChar = (toUpperStr str) !! 0
          secondChar =  (toUpperStr str) !! 1


-- Converts a chess move from the "[a-h][1-8] to [a-h][1-8]" format to a ChessMove
-- WARNING: Assumes that the string is in a valid format
stringToChessMove :: String -> ChessMove
stringToChessMove string = 
    ChessMove (readChessPosition fromPosition) (readChessPosition toPosition)
    where
        fromPosition = take 2 [ x | x <- lowerString, x /=' ', x /='t', x/='o']
        toPosition = drop 2 [ x | x <- lowerString, x /=' ', x /='t', x/='o'] 
        lowerString = [toLower ch | ch <- string]

readChessPosition :: [Char] -> (Char, Integer)
readChessPosition string = (string !! 0, toInteger (digitToInt (string !! 1)))

