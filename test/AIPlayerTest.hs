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


-- Will not pass until makeMove is implemented
-- TODO: add more tests [Aziz]
test_buildGameTree =
    do
        assertEqual gameTree16_depth0_White (buildGameTree board16 White 0)
        assertEqual gameTree16_depth1_White (buildGameTree board16 White 1)
        assertEqual gameTree16_depth0_Black (buildGameTree board16 Black 0)
        assertEqual gameTree16_depth1_Black (buildGameTree board16 Black 1)


test_maximize =
    do
        assertEqual gameTree16_depth0_White (maximize gameTree16_depth0_White White)
        assertEqual gameTree16_depth0_Black (maximize gameTree16_depth0_Black Black)

test_minimize =
    do
        assertEqual gameTree16_depth0_White (minimize gameTree16_depth0_White White)
        assertEqual gameTree16_depth0_Black (minimize gameTree16_depth0_Black Black)

cloneTree (GameTree board score children) = (GameTree board score (map cloneMoveSubtree children))
cloneMoveSubtree (MoveSubtree move tree) = (MoveSubtree move (cloneTree tree))

main = htfMain htf_thisModulesTests