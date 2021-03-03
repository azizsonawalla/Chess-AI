{-# OPTIONS_GHC -F -pgmF htfpp #-}

import Test.Framework
import AIPlayer
import TestChessBoards
import TestGameTrees
import ChessUtilTypes
import ChessBoard

test_GameTreeEq = 
    do
        assertEqual True (gameTree16_depth0_White == (cloneTree gameTree16_depth0_White))
        assertEqual True (gameTree16_depth0_Black == (cloneTree gameTree16_depth0_Black))
        assertEqual True (gameTree16_depth1_White == (cloneTree gameTree16_depth1_White))
        assertEqual True (gameTree16_depth1_Black == (cloneTree gameTree16_depth1_Black))
        assertEqual False (gameTree16_depth1_White == gameTree16_depth0_White)
        assertEqual False (gameTree16_depth1_White == gameTree16_depth1_Black)
        assertEqual False (gameTree16_depth1_Black == gameTree16_depth0_Black)


test_buildGameTree =
    do
        assertEqual gameTree16_depth0_White (buildGameTree board16 White 0)
        assertEqual gameTree16_depth1_White (buildGameTree board16 White 1)
        assertEqual gameTree16_depth0_Black (buildGameTree board16 Black 0)
        assertEqual gameTree16_depth1_Black (buildGameTree board16 Black 1)


test_maximize =
    do
        assertEqual gameTree16_depth1_White_scores_max (maximize gameTree16_depth1_White dummyScoreFn)
        assertEqual gameTree16_depth1_Black_scores_max (maximize gameTree16_depth1_Black dummyScoreFn)


test_minimize =
    do
        assertEqual gameTree16_depth1_White_scores_min (minimize gameTree16_depth1_White dummyScoreFn)
        assertEqual gameTree16_depth1_Black_scores_min (minimize gameTree16_depth1_Black dummyScoreFn)


test_score = 
    do
        assertEqual 890 (score Black board2)
        assertEqual (-890) (score White board2)
        assertEqual (-80) (score Black board3)
        assertEqual 80 (score White board3)


test_getMoveWithMaxScore = 
    do 
        assertEqual board16_W1_Move (getMoveWithMaxScore gameTree16_depth1_White_scores_max)
        assertEqual board16_B8_Move (getMoveWithMaxScore gameTree16_depth1_Black_scores_max)


-- A dummy score function
dummyScoreFn :: ChessBoard -> Integer
dummyScoreFn board 
    | board == board16_W1_Board = 50
    | board == board16_W2_Board = -1
    | board == board16_B1_Board = 1
    | board == board16_B2_Board = -32
    | board == board16_B3_Board = 50
    | board == board16_B4_Board = -81
    | board == board16_B5_Board = 200
    | board == board16_B6_Board = 500
    | board == board16_B7_Board = -800
    | board == board16_B8_Board = 501
    | otherwise                 = -999999

-- Clone a GameTree
cloneTree :: GameTree -> GameTree
cloneTree (GameTree board score children) = (GameTree board score (map cloneMoveSubtree children))


-- Clone a MoveSubtree
cloneMoveSubtree :: MoveSubtree -> MoveSubtree
cloneMoveSubtree (MoveSubtree move tree) = (MoveSubtree move (cloneTree tree))


main = htfMain htf_thisModulesTests