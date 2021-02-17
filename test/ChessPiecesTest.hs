{-# OPTIONS_GHC -F -pgmF htfpp #-}

import Test.Framework
import ChessPieces
import ChessUtilTypes

test_buildMoves = 
    do
        assertEqual [] (buildMoves ('A', 8) [])
        assertEqual [(ChessMove ('B', 2) ('A', 1)), (ChessMove ('B', 2) ('B', 3))] (buildMoves ('B', 2) [('A', 1), ('B', 3)])

test_getPieceColour = 
    do
        assertEqual White (getPieceColour (Rook White))
        assertEqual Black (getPieceColour (King Black))
        assertEqual White (getPieceColour (Bishop White))
        assertEqual Black (getPieceColour (Queen Black))
        assertEqual White (getPieceColour (Knight White))
        assertEqual Black (getPieceColour (Pawn Black False))


main = htfMain htf_thisModulesTests