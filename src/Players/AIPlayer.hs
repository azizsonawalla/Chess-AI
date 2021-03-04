module AIPlayer where

import ChessUtilTypes
import Data.List (sort)
import Data.Tree
import ChessBoard
import ChessPieces

-- A null chess move
nullMove :: ChessMove
nullMove = ChessMove ('Z', -1) ('Z', -1)


-- AI Player search depth
searchDepth :: Integer
searchDepth = 4


-- Makes the AI player's move on the chess board.
-- Calculates the best next move and changes the board accordingly.
-- Returns the changed board after playing the move.
aiMoveFunction :: MoveFunction
aiMoveFunction chessPieceColour chessBoard = 
    do 
        putStrLn (show chessBoard)                                                  -- show the user the chess board
        putStrLn "AI's turn. Please wait."                                          -- TODO: if no legal moves are available, set game to 'over'
        move <- getBestMoveMinMax chessBoard chessPieceColour
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
getBestMoveMinMax :: ChessBoard -> ChessPieceColour -> IO ChessMove
getBestMoveMinMax chessBoard pieceColour = 
    do 
       let gameTree = buildGameTree chessBoard pieceColour searchDepth
       let maximizedTree = maximize gameTree (score pieceColour)
       putStrLn (drawTree (gameTreeToDataTree nullMove (pruneGameTree maximizedTree 2)))
       return (getMoveWithMaxScore maximizedTree)


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


-- A score function for a particular colour
type ScoreColour = (ChessBoard -> Integer)

-- Scores the given GameTree using a score maximizing strategy
-- If tree has no children, scores the root (using given colour) and returns new game tree with the score calculated
-- If tree has children, calls minimize on each subtree (with the opposite colour) and returns the root where the score is the max of the children's scores
-- Reference: https://www.javatpoint.com/mini-max-algorithm-in-ai
maximize :: GameTree -> ScoreColour -> GameTree
maximize (GameTree chessBoard _ []) scoreFn = GameTree chessBoard (scoreFn chessBoard) []
maximize (GameTree chessBoard _ children) scoreFn = GameTree chessBoard minChildScore maximizedChildren
    where
        maximizedChildren = map (\ (MoveSubtree move childtree) -> (MoveSubtree move (minimize childtree scoreFn))) children
        childScores = [ childScore | (MoveSubtree _ (GameTree _ childScore _)) <- maximizedChildren ]
        minChildScore = maximum childScores


-- Scores the given GameTree using a score minimization strategy
-- If tree has no children, scores the root (using the opposite of the given colour) and returns new game tree with the score calculated
-- If tree has children, calls maximize on each subtree (with the opposite colour) and returns the root where the score is the min of the children's scores
-- Reference: https://www.javatpoint.com/mini-max-algorithm-in-ai
minimize :: GameTree -> ScoreColour -> GameTree
minimize (GameTree chessBoard _ []) scoreFn = GameTree chessBoard (scoreFn chessBoard) []
minimize (GameTree chessBoard _ children) scoreFn = GameTree chessBoard minChildScore maximizedChildren
    where
        maximizedChildren = map (\ (MoveSubtree move childtree) -> (MoveSubtree move (maximize childtree scoreFn))) children
        childScores = [ childScore | (MoveSubtree _ (GameTree _ childScore _)) <- maximizedChildren ]
        minChildScore = minimum childScores


-- Returns the next move that maximizes the score
getMoveWithMaxScore :: GameTree -> ChessMove
getMoveWithMaxScore (GameTree _ rootScore nextMoves) = moveOfMaxScore
    where movesWithScore = [(move, (getScore tree)) | (MoveSubtree move tree) <- nextMoves]        -- nextMoves :: [MoveSubtree]
          moveWithMaxScore = (filter (\ (move, score) -> score == rootScore) movesWithScore) !! 0  -- after calling maximize, root has same score as max subtree
          moveOfMaxScore = getMove moveWithMaxScore


-- returns the ChessMove of the given pair
getMove :: (ChessMove, Score) -> ChessMove
getMove (move, _) = move

-- returns the score of the given tree
getScore :: GameTree -> Score
getScore (GameTree _ score _) = score


-- Scores the given chess board based on how beneficial the positions are for the given colour
-- The higher the score, the more advantageous the scenario is for the colour
-- First version: score is the sum of the scores for each piece on the board of the colour minus the scores for each piece of the other colour on the board
-- Scores per piece: https://cdn-media-1.freecodecamp.org/images/1*e4p9BrCzJUdlqx7KVGW9aA.png
score :: ChessPieceColour -> ChessBoard -> Integer
score colour (ChessBoard pieces _) = scoreOfCurrColour - scoreOfOppoColour
    where currColour = filter (\ (position, piece) -> (getPieceColour piece) == colour) pieces
          oppoColour = filter (\ (position, piece) -> (getPieceColour piece) /= colour) pieces
          scoreOfCurrColour = foldr (+) 0 (map (\ (position, piece) -> (pieceToScore piece)) currColour)
          scoreOfOppoColour = foldr (+) 0 (map (\ (position, piece) -> (pieceToScore piece)) oppoColour) 


-- Returns the score of the given piece
pieceToScore :: ChessPiece -> Integer
pieceToScore (King _)   = 900
pieceToScore (Queen _)  = 90
pieceToScore (Rook _)   = 50
pieceToScore (Bishop _) = 30
pieceToScore (Knight _) = 30
pieceToScore (Pawn _)   = 10


-- Prune the depth of a GameTree
pruneGameTree :: GameTree -> Integer -> GameTree
pruneGameTree (GameTree board score children) 1 = GameTree board score []
pruneGameTree (GameTree board score children) depth = GameTree board score [ (MoveSubtree move (pruneGameTree childTree (depth-1))) | (MoveSubtree move childTree) <- children]


-- Translates a GameTree to a Data.Tree
gameTreeToDataTree :: ChessMove -> GameTree -> Tree String
gameTreeToDataTree move (GameTree _ score children) = Node label forest 
    where label = if move == nullMove then "current board (score="++(show score)++")" else (show move)++"(score="++(show score)++")"
          forest = (map (\ (MoveSubtree move gt) -> gameTreeToDataTree move gt) children)