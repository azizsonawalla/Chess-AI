{-# OPTIONS_GHC -F -pgmF htfpp #-}

import Test.Framework
import HumanPlayer
import ChessUtilTypes
import TestChessBoards
import System.IO
import FENotation

test_stringToChessMove = 
    do
        assertEqual (ChessMove ('A', 1) ('A', 3)) (stringToChessMove "a1 to a3")
        assertEqual (ChessMove ('D', 7) ('C', 5)) (stringToChessMove "d7 to c5")
        assertEqual (ChessMove ('b', 4) ('E', 2)) (stringToChessMove "b4 to e2")
        assertEqual (ChessMove ('H', 4) ('H', 4)) (stringToChessMove "h4 to h4")
        assertEqual (ChessMove ('F', 8) ('A', 1)) (stringToChessMove "f8 to a1")
        assertEqual (ChessMove ('C', 4) ('G', 4)) (stringToChessMove "c4 to g5")


main = htfMain htf_thisModulesTests