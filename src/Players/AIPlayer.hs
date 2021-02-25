module AIPlayer where

import ChessUtilTypes
import Data.List (sort)
import ChessBoard

-- Makes the AI player's move on the chess board.
-- Calculates the best next move and changes the board accordingly.
-- Returns the changed board after playing the move.
aiMoveFunction :: MoveFunction
aiMoveFunction chessPieceColour chessBoard = 
    do 
        putStrLn "AI's turn. Please wait."                                  -- TODO: if no legal moves are available, set game to 'over'
        let move = getBestMoveRandom chessBoard chessPieceColour
        let newChessBoard = makeMove chessBoard move
        return (newChessBoard, move)


-- Returns a random next move
-- chessboard (ChessBoard):    the current board
-- colour (ChessPieceColour):  the colour/side of the current player
getBestMoveRandom :: ChessBoard -> ChessPieceColour -> ChessMove
getBestMoveRandom chessBoard pieceColour = moves !! middleIdx where moves = legalMoves chessBoard pieceColour
                                                                    middleIdx = div (length moves) 2


-- Analyzes the board and returns the best move to make (minmax algorithm)
-- chessboard (ChessBoard):    the current board
-- colour (ChessPieceColour):  the colour/side of the current player
-- TODO: add tests [Aziz]
getBestMoveMinMax :: ChessBoard -> ChessPieceColour -> ChessMove
getBestMoveMinMax chessBoard pieceColour = getMoveWithMaxScore maximizedTree
    where maximizedTree = maximize gameTree pieceColour
          gameTree = buildGameTree chessBoard pieceColour 3    -- analyzes to depth=3


-- A tree representing all possible outcomes starting from the root chessboard
-- All scores are initially 0
type Score = Integer
data MoveSubtree = MoveSubtree ChessMove GameTree deriving (Eq, Ord, Show)              -- a legal move from root and the resulting subtree
data GameTree = GameTree ChessBoard Score [MoveSubtree] deriving (Ord, Show)
instance Eq GameTree where -- 2 GameTrees are equal if the roots are equal and the children are pairwise equal
    (GameTree c1 s1 m1) == (GameTree c2 s2 m2) = (c1 == c2) && (s1 == s2) && sameNumOfChildren && childrenAreSame
        where sameNumOfChildren = (length m1) == (length m2)
              childrenAreSame = foldr (\ (child1, child2) acc -> acc && (child1 == child2)) True zippedChildren
              zippedChildren = zip (sort m1) (sort m2)


-- Builds a GameTree of the given depth starting from the given board as root
-- The given colour makes the next move
-- TODO: implement + test this [Aziz]
buildGameTree :: ChessBoard -> ChessPieceColour -> Integer -> GameTree
buildGameTree chessBoard _ 0 = GameTree chessBoard 0 []
buildGameTree chessBoard colour depth = GameTree chessBoard 0 children
    where children = map moveToMoveSubtree nextMoves
          moveToMoveSubtree move = MoveSubtree move (buildChild move)
          buildChild move = buildGameTree (makeMove chessBoard move) oppColour (depth-1)
          nextMoves = legalMoves chessBoard colour
          oppColour = oppositeColour colour


-- Scores the given GameTree using a score maximizing strategy
-- If tree has no children, scores the root (using given colour) and returns new game tree with the score calculated
-- If tree has children, calls minimize on each subtree (with the opposite colour) and returns the root where the score is the max of the children's scores
-- Reference: https://www.javatpoint.com/mini-max-algorithm-in-ai
maximize :: GameTree -> ChessPieceColour -> GameTree
maximize (GameTree chessBoard score children) colour
    | null children             = GameTree chessBoard nodeScore children
    | otherwise                 = GameTree chessBoard maxScore children
    where   
        nodeScore = score chessBoard colour
        maxScore = foldr (\currMax currScore -> if currScore > currMax then currScore else currMax ) 0 allScores
        allScores = [ subScore | (GameTree _ subScore _) <- subtreeList ]
        subtreeList = [ minimize subtree (oppositeColour colour) | (MoveSubtree move subtree) <- children ]



-- Scores the given GameTree using a score minimization strategy
-- If tree has no children, scores the root (using the opposite of the given colour) and returns new game tree with the score calculated
-- If tree has children, calls maximize on each subtree (with the opposite colour) and returns the root where the score is the min of the children's scores
-- Reference: https://www.javatpoint.com/mini-max-algorithm-in-ai
minimize :: GameTree -> ChessPieceColour -> GameTree
minimize (GameTree chessBoard score children) colour 
    | null children             = GameTree chessBoard nodeScore children
    | otherwise                 = GameTree chessBoard minScore children
    where   
        nodeScore = score chessBoard (oppositeColour colour)
        minScore = foldr (\currMin currScore -> if currScore < currMin then currScore else currMin ) 0 allScores
        allScores = [ subScore | (GameTree _ subScore _) <- subtreeList ]
        subtreeList = [ maximize subtree (oppositeColour colour) | (MoveSubtree move subtree) <- children ]


-- Returns the next move that maximizes the score
-- TODO: implement + test this [Yiyi]
getMoveWithMaxScore :: GameTree -> ChessMove
getMoveWithMaxScore gameTree = ChessMove ('z', -1) ('z', -1)


-- Scores the given chess board based on how beneficial the positions are for the given colour
-- The higher the score, the more advantageous the scenario is for the colour
-- First version: score is the sum of the scores for each piece on the board of the colour minus the scores for each piece of the other colour on the board
-- Scores per piece: https://cdn-media-1.freecodecamp.org/images/1*e4p9BrCzJUdlqx7KVGW9aA.png
-- TODO: implement + test this [Yiyi]
score :: ChessBoard -> ChessPieceColour -> Score
score chessBoard forColour = -1


-- Returns the opposite of the given colour
oppositeColour :: ChessPieceColour -> ChessPieceColour
oppositeColour White = Black
oppositeColour Black = White