{-# OPTIONS_GHC -F -pgmF htfpp #-}

import Test.Framework
import ChessUtilTypes

test_showChessMove = 
    do
        assertEqual "A1 to G3" (show (ChessMove ('A', 1) ('G', 3)))
        assertEqual "F7 to B6" (show (ChessMove ('F', 7) ('B', 6)))


main = htfMain htf_thisModulesTests