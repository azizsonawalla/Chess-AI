{-# OPTIONS_GHC -F -pgmF htfpp #-}

import Test.Framework
import ChessPieces
import ChessUtilTypes

test_buildMoves = 
    do
        assertEqual [] (buildMoves ('A', 8) [])
        assertEqual [(ChessMove ('B', 2) ('A', 1)), (ChessMove ('B', 2) ('B', 3))] (buildMoves ('B', 2) [('A', 1), ('B', 3)])


main = htfMain htf_thisModulesTests