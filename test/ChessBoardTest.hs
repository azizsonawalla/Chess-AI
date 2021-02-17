{-# OPTIONS_GHC -F -pgmF htfpp #-}

import Test.Framework
import ChessBoard
import ChessUtilTypes

test_getPieceAt = 
    do 
        assertEqual (Just (Rook White)) (getPieceAt ('A', 1) freshBoard)
        assertEqual (Just (King White)) (getPieceAt ('D', 1) freshBoard)
        assertEqual (Just (Pawn Black)) (getPieceAt ('G', 7) freshBoard)
        assertEqual (Nothing) (getPieceAt ('A', 5) freshBoard)
        assertEqual (Nothing) (getPieceAt ('E', 4) freshBoard)

test_gameOver = 
    do
        assertEqual False (gameOver (ChessBoard [] Ongoing))
        assertEqual True (gameOver (ChessBoard [] Over))

main = htfMain htf_thisModulesTests