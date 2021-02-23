module AIPlayer where

import ChessUtilTypes
import ChessBoard

-- Makes the AI player's move on the chess board.
-- Calculates the best next move and changes the board accordingly.
-- Returns the changed board after playing the move.
aiMoveFunction :: MoveFunction
aiMoveFunction chessPieceColour chessBoard = 
    do 
        putStrLn "AI's turn. Please wait."
        let move = getBestMoveRandom chessBoard chessPieceColour
        let newChessBoard = makeMove chessBoard move
        return (newChessBoard, move)


-- Analyzes the board and returns the best move to make
-- chessboard (ChessBoard):    the current board
-- colour (ChessPieceColour):  the colour/side of the current player
-- TODO: Second round = minmax algorithm [Aziz]
getBestMoveRandom :: ChessBoard -> ChessPieceColour -> ChessMove
getBestMoveRandom chessBoard pieceColour = moves !! middleIdx where moves = legalMoves chessBoard pieceColour   -- TODO: handle case where moves is empty
                                                              middleIdx = div (length moves) 2


-- A tree representing all possible outcomes starting from the root chessboard
-- All scores are initially -1
type Score = Integer
data MoveSubtree = MoveSubtree ChessMove GameTree               -- a legal move from root and the resulting subtree
data GameTree = GameTree ChessBoard Score [MoveSubtree]


-- Builds a GameTree of the given depth starting from the given board as root
-- The given colour makes the next move
-- TODO: implement + test this [Aziz]
buildGameTree :: ChessBoard -> ChessPieceColour -> Integer -> GameTree
buildGameTree chessBoard _ 0 = GameTree chessBoard (-1) []
buildGameTree chessBoard colour depth = GameTree chessBoard (-1) []


-- Scores the given GameTree using a score maximizing strategy
-- If tree has no children, scores the root (using given colour) and returns new game tree with the score calculated
-- If tree has children, calls minimize on each subtree (with the opposite colour) and returns the root where the score is the max of the children's scores
-- Reference: https://www.javatpoint.com/mini-max-algorithm-in-ai
-- TODO: implement and test this [Cynthia]
maximize :: GameTree -> ChessPieceColour -> GameTree
maximize (GameTree chessBoard score children) colour = (GameTree chessBoard score children)


-- Scores the given GameTree using a score minimization strategy
-- If tree has no children, scores the root (using the opposite of the given colour) and returns new game tree with the score calculated
-- If tree has children, calls maximize on each subtree (with the opposite colour) and returns the root where the score is the min of the children's scores
-- Reference: https://www.javatpoint.com/mini-max-algorithm-in-ai
-- TODO: implement and test this [Cynthia]
minimize :: GameTree -> ChessPieceColour -> GameTree
minimize (GameTree chessBoard score children) colour = (GameTree chessBoard score children)


-- Returns the next move that maximizes the score
-- TODO: implement + test this [Yiyi]
getMoveWithMaxScore :: GameTree -> ChessMove
getMoveWithMaxScore gameTree = ChessMove ('z', -1) ('z', -1)


-- Scores the given chess board based on how beneficial the positions are for the given colour
-- The higher the score, the more advantageous the scenario is for the colour
-- First version: score is the sum of the scores for each piece on the board of the colour minus the scores for each piece of the other colour on the board
-- Scores per piece: https://cdn-media-1.freecodecamp.org/images/1*e4p9BrCzJUdlqx7KVGW9aA.png
-- TODO: implement + test this [Yiyi]
score :: ChessBoard -> ChessPieceColour -> Integer
score chessBoard forColour = -1


-- Returns the opposite of the given colour
oppositeColour :: ChessPieceColour -> ChessPieceColour
oppositeColour White = Black
oppositeColour Black = White