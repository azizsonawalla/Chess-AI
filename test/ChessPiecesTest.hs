{-# OPTIONS_GHC -F -pgmF htfpp #-}

import Test.Framework
import ChessPieces
import ChessUtilTypes
import ChessBoard
import TestChessBoards

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
        assertEqual Black (getPieceColour (Pawn Black))

test_getTopLeftDiagonal = 
    do
        assertEqual [('C', 3), ('B', 4), ('A', 5)] (getTopLeftDiagonal ('D', 2))

test_legalNextPosForPieceAtPos_Bishop = 
    do
        -- freshboard
        assertListsEqualAsSets freshBoard_C1 (legalNextPosForPieceAtPos (Bishop White) freshBoard ('C', 1))
        assertListsEqualAsSets freshBoard_F1 (legalNextPosForPieceAtPos (Bishop White) freshBoard ('F', 1))
        assertListsEqualAsSets freshBoard_C8 (legalNextPosForPieceAtPos (Bishop Black) freshBoard ('C', 8))
        assertListsEqualAsSets freshBoard_F8 (legalNextPosForPieceAtPos (Bishop Black) freshBoard ('F', 8))
        -- board2
        assertListsEqualAsSets board2_E1 (legalNextPosForPieceAtPos (Bishop White) board2 ('E', 1))
        -- board3
        assertListsEqualAsSets board3_E3 (legalNextPosForPieceAtPos (Bishop Black) board3 ('E', 3))
        -- board11
        assertListsEqualAsSets board11_D2 (legalNextPosForPieceAtPos (Bishop White) board11 ('D', 2))
        assertListsEqualAsSets board11_H2 (legalNextPosForPieceAtPos (Bishop Black) board11 ('H', 2))
        assertListsEqualAsSets board11_A3 (legalNextPosForPieceAtPos (Bishop White) board11 ('A', 3))
        assertListsEqualAsSets board11_B4 (legalNextPosForPieceAtPos (Bishop White) board11 ('B', 4))
        assertListsEqualAsSets board11_F4 (legalNextPosForPieceAtPos (Bishop White) board11 ('F', 4))
        assertListsEqualAsSets board11_D6 (legalNextPosForPieceAtPos (Bishop Black) board11 ('D', 6))
        assertListsEqualAsSets board11_H6 (legalNextPosForPieceAtPos (Bishop White) board11 ('H', 6))
        assertListsEqualAsSets board11_A7 (legalNextPosForPieceAtPos (Bishop Black) board11 ('A', 7))
        assertListsEqualAsSets board11_C7 (legalNextPosForPieceAtPos (Bishop White) board11 ('C', 7))
        assertListsEqualAsSets board11_F8 (legalNextPosForPieceAtPos (Bishop Black) board11 ('F', 8))


test_legalNextPosForPieceAtPos_King = 
    do
        -- E1 white king at starting position (freshboard)
        assertEqual [] (legalNextPosForPieceAtPos (King White) freshBoard ('E', 1))
        -- E8 black king at starting position (freshboard)
        assertEqual [] (legalNextPosForPieceAtPos (King Black) freshBoard ('E', 8))
        -- D8 black king with no adjacent pieces
        assertEqual board2_D8 (legalNextPosForPieceAtPos (King Black) board2 ('D', 8))
        -- A1 white king in left corner with no adjacent pieces
        assertEqual board12_A1 (legalNextPosForPieceAtPos (King White) board12 ('A', 1))
        -- E5 black king in middle of board enemy bishop above, friendly Queen below and friendly pawn top left
        assertEqual board12_E5 (legalNextPosForPieceAtPos (King Black) board12 ('E', 5))
        -- A1 white king in left corner with rook top right
        assertEqual board13_A1 (legalNextPosForPieceAtPos (King White) board13 ('A', 1))
        -- A8 black king in upper left corner
        assertEqual board13_A8 (legalNextPosForPieceAtPos (King Black) board13 ('A', 8))


test_legalNextPosForPieceAtPos_Rook = 
    do
        -- A1 white rook at starting position (freshboard)
        assertEqual [] (legalNextPosForPieceAtPos (Rook White) freshBoard ('A', 1))
        -- H1 white rook at starting position (freshboard)
        assertEqual [] (legalNextPosForPieceAtPos (Rook White) freshBoard ('H', 1))
        -- H8 black rook at starting position (freshboard)
        assertEqual [] (legalNextPosForPieceAtPos (Rook Black) freshBoard ('H', 8))
        -- A8 black rook at starting position (freshboard)
        assertEqual [] (legalNextPosForPieceAtPos (Rook Black) freshBoard ('A', 8))
        -- A1 white rook only piece on board
        assertEqual board1_A1 (legalNextPosForPieceAtPos (Rook White) board1 ('A', 1))
        -- A2 white rook with no adjacent pieces
        assertEqual board2_A2 (legalNextPosForPieceAtPos (Rook White) board2 ('A', 2))
        -- H3 black rook with enemy pawn below, enemy knight above, friendly bishop to the left
        assertEqual board3_H3 (legalNextPosForPieceAtPos (Rook Black) board3 ('H', 3))
        -- A6 white rook with enemy pawn below, enemy pawn above, friendly queen to the right
        assertEqual board3_A6 (legalNextPosForPieceAtPos (Rook White) board3 ('A', 6))
        -- B2 white rook with friendly pawn above and king left bottom
        assertEqual board13_B2 (legalNextPosForPieceAtPos (Rook White) board13 ('B', 2))
        -- C6 black rook with enemy queen on top and friendly pawn bottom left
        assertEqual board13_C6 (legalNextPosForPieceAtPos (Rook Black) board13 ('C', 6))
test_legalNextPosForPieceAtPos_Knight = 
    do
        -- board 14
        assertListsEqualAsSets board14_A1 (legalNextPosForPieceAtPos (Knight White) board14 ('A', 1))
        assertListsEqualAsSets board14_H1 (legalNextPosForPieceAtPos (Knight White) board14 ('H', 1))
        assertListsEqualAsSets board14_F3 (legalNextPosForPieceAtPos (Knight White) board14 ('F', 3))
        assertListsEqualAsSets board14_D5 (legalNextPosForPieceAtPos (Knight White) board14 ('D', 5))
        assertListsEqualAsSets board14_B6 (legalNextPosForPieceAtPos (Knight White) board14 ('B', 6))
        assertListsEqualAsSets board14_H8 (legalNextPosForPieceAtPos (Knight White) board14 ('H', 8))
        assertListsEqualAsSets board14_A8 (legalNextPosForPieceAtPos (Knight White) board14 ('A', 8))
        -- board 15
        assertListsEqualAsSets board15_A1 (legalNextPosForPieceAtPos (Knight Black) board15 ('A', 1))
        assertListsEqualAsSets board15_H1 (legalNextPosForPieceAtPos (Knight Black) board15 ('H', 1))
        assertListsEqualAsSets board15_F3 (legalNextPosForPieceAtPos (Knight Black) board15 ('F', 3))
        assertListsEqualAsSets board15_D5 (legalNextPosForPieceAtPos (Knight Black) board15 ('D', 5))
        assertListsEqualAsSets board15_B6 (legalNextPosForPieceAtPos (Knight Black) board15 ('B', 6))
        assertListsEqualAsSets board15_H8 (legalNextPosForPieceAtPos (Knight Black) board15 ('H', 8))
        assertListsEqualAsSets board15_A8 (legalNextPosForPieceAtPos (Knight Black) board15 ('A', 8))


test_legalNextPosForPieceAtPos_Pawn = 
    do
        -- A2 pawn at starting position (freshboard)
        assertListsEqualAsSets freshBoard_A2 (legalNextPosForPieceAtPos (Pawn White) freshBoard ('A', 2))
        -- A2 pawn at starting position with opposite piece in front (board3)
        assertListsEqualAsSets board3_A2 (legalNextPosForPieceAtPos (Pawn White) board3 ('A', 2))
        -- A2 pawn at starting position with same colour piece in front (board4)
        assertListsEqualAsSets board4_A2 (legalNextPosForPieceAtPos (Pawn White) board4 ('A', 2))
        -- A2 pawn at starting position with opposite piece diagonally in front (board5)
        assertListsEqualAsSets board5_A2 (legalNextPosForPieceAtPos (Pawn White) board5 ('A', 2))
        -- A2 pawn at starting position with same colour piece diagonally in front (board6)
        assertListsEqualAsSets board6_A2 (legalNextPosForPieceAtPos (Pawn White) board6 ('A', 2))
        -- A2 pawn at starting position with opposite piece one square away (board7)
        assertListsEqualAsSets board7_A2 (legalNextPosForPieceAtPos (Pawn White) board7 ('A', 2))
        -- A2 pawn at starting position with same colour piece one square away (board8)
        assertListsEqualAsSets board8_A2 (legalNextPosForPieceAtPos (Pawn White) board8 ('A', 2))

        -- E2 pawn at starting position (freshboard)
        assertListsEqualAsSets freshBoard_E2 (legalNextPosForPieceAtPos (Pawn White) freshBoard ('E', 2))
        -- E2 pawn at starting position with opposite piece in front (board3)
        assertListsEqualAsSets board3_E2 (legalNextPosForPieceAtPos (Pawn White) board3 ('E', 2))
        -- E2 pawn at starting position with same colour piece in front (board4)
        assertListsEqualAsSets board4_E2 (legalNextPosForPieceAtPos (Pawn White) board4 ('E', 2))
        -- E2 pawn at starting position with opposite piece diagonally in front (board5)
        assertListsEqualAsSets board5_E2 (legalNextPosForPieceAtPos (Pawn White) board5 ('E', 2))
        -- E2 pawn at starting position with same colour piece diagonally in front (board6)
        assertListsEqualAsSets board6_E2 (legalNextPosForPieceAtPos (Pawn White) board6 ('E', 2))
        -- E2 pawn at starting position with opposite piece one square away (board7)
        assertListsEqualAsSets board7_E2 (legalNextPosForPieceAtPos (Pawn White) board7 ('E', 2))
        -- E2 pawn at starting position with same colour piece one square away (board8)
        assertListsEqualAsSets board8_E2 (legalNextPosForPieceAtPos (Pawn White) board8 ('E', 2))

        -- H2 pawn at starting position (freshboard)
        assertListsEqualAsSets freshBoard_H2 (legalNextPosForPieceAtPos (Pawn White) freshBoard ('H', 2))
        -- H2 pawn at starting position with opposite piece in front (board3)
        assertListsEqualAsSets board3_H2 (legalNextPosForPieceAtPos (Pawn White) board3 ('H', 2))
        -- H2 pawn at starting position with same colour piece in front (board4)
        assertListsEqualAsSets board4_H2 (legalNextPosForPieceAtPos (Pawn White) board4 ('H', 2))
        -- H2 pawn at starting position with opposite piece diagonally in front (board5)
        assertListsEqualAsSets board5_H2 (legalNextPosForPieceAtPos (Pawn White) board5 ('H', 2))
        -- H2 pawn at starting position with same colour piece diagonally in front (board6)
        assertListsEqualAsSets board6_H2 (legalNextPosForPieceAtPos (Pawn White) board6 ('H', 2))
        -- H2 pawn at starting position with opposite piece one square away (board7)
        assertListsEqualAsSets board7_H2 (legalNextPosForPieceAtPos (Pawn White) board7 ('H', 2))
        -- H2 pawn at starting position with same colour piece one square away (board8)
        assertListsEqualAsSets board8_H2 (legalNextPosForPieceAtPos (Pawn White) board8 ('H', 2))
        
        -- A7 pawn at starting position (freshboard)
        assertListsEqualAsSets freshBoard_A7 (legalNextPosForPieceAtPos (Pawn Black) freshBoard ('A', 7))
        -- A7 pawn at starting position with opposite piece in front (board3)
        assertListsEqualAsSets board3_A7 (legalNextPosForPieceAtPos (Pawn Black) board3 ('A', 7))
        -- A7 pawn at starting position with same colour piece in front (board4)
        assertListsEqualAsSets board4_A7 (legalNextPosForPieceAtPos (Pawn Black) board4 ('A', 7))
        -- A7 pawn at starting position with opposite piece diagonally in front (board5)
        assertListsEqualAsSets board5_A7 (legalNextPosForPieceAtPos (Pawn Black) board5 ('A', 7))
        -- A7 pawn at starting position with same colour piece diagonally in front (board6)
        assertListsEqualAsSets board6_A7 (legalNextPosForPieceAtPos (Pawn Black) board6 ('A', 7))
        -- A7 pawn at starting position with opposite piece one square away (board7)
        assertListsEqualAsSets board7_A7 (legalNextPosForPieceAtPos (Pawn Black) board7 ('A', 7))
        -- A7 pawn at starting position with same colour piece one square away (board8)
        assertListsEqualAsSets board8_A7 (legalNextPosForPieceAtPos (Pawn Black) board8 ('A', 7))

        -- E7 pawn at starting position (freshboard)
        assertListsEqualAsSets freshBoard_E7 (legalNextPosForPieceAtPos (Pawn Black) freshBoard ('E', 7))
        -- E7 pawn at starting position with opposite piece in front (board3)
        assertListsEqualAsSets board3_E7 (legalNextPosForPieceAtPos (Pawn Black) board3 ('E', 7))
        -- E7 pawn at starting position with same colour piece in front (board4)
        assertListsEqualAsSets board4_E7 (legalNextPosForPieceAtPos (Pawn Black) board4 ('E', 7))
        -- E7 pawn at starting position with opposite piece diagonally in front (board5)
        assertListsEqualAsSets board5_E7 (legalNextPosForPieceAtPos (Pawn Black) board5 ('E', 7))
        -- E7 pawn at starting position with same colour piece diagonally in front (board6)
        assertListsEqualAsSets board6_E7 (legalNextPosForPieceAtPos (Pawn Black) board6 ('E', 7))
        -- E7 pawn at starting position with opposite piece one square away (board7)
        assertListsEqualAsSets board7_E7 (legalNextPosForPieceAtPos (Pawn Black) board7 ('E', 7))
        -- E7 pawn at starting position with same colour piece one square away (board8)
        assertListsEqualAsSets board8_E7 (legalNextPosForPieceAtPos (Pawn Black) board8 ('E', 7))

        -- H7 pawn at starting position (freshboard)
        assertListsEqualAsSets freshBoard_H7 (legalNextPosForPieceAtPos (Pawn Black) freshBoard ('H', 7))
        -- H7 pawn at starting position with opposite piece in front (board3)
        assertListsEqualAsSets board3_H7 (legalNextPosForPieceAtPos (Pawn Black) board3 ('H', 7))
        -- H7 pawn at starting position with same colour piece in front (board4)
        assertListsEqualAsSets board4_H7 (legalNextPosForPieceAtPos (Pawn Black) board4 ('H', 7))
        -- H7 pawn at starting position with opposite piece diagonally in front (board5)
        assertListsEqualAsSets board5_H7 (legalNextPosForPieceAtPos (Pawn Black) board5 ('H', 7))
        -- H7 pawn at starting position with same colour piece diagonally in front (board6)
        assertListsEqualAsSets board6_H7 (legalNextPosForPieceAtPos (Pawn Black) board6 ('H', 7))
        -- H7 pawn at starting position with opposite piece one square away (board7)
        assertListsEqualAsSets board7_H7 (legalNextPosForPieceAtPos (Pawn Black) board7 ('H', 7))
        -- H7 pawn at starting position with same colour piece one square away (board8)
        assertListsEqualAsSets board8_H7 (legalNextPosForPieceAtPos (Pawn Black) board8 ('H', 7))

        -- White:
            -- Non starting position no pieces in front (board9)
        assertListsEqualAsSets board9_A3 (legalNextPosForPieceAtPos (Pawn White) board9 ('A', 3))
            -- Non starting position same colour pieces directly in front (board9)
        assertListsEqualAsSets board9_G3 (legalNextPosForPieceAtPos (Pawn White) board9 ('G', 3))
            -- Non starting position same colour pieces diagonally in front (board9)
        assertListsEqualAsSets board9_D4 (legalNextPosForPieceAtPos (Pawn White) board9 ('D', 4))
            -- Non starting position opposite colour pieces directly in front (board9)
        assertListsEqualAsSets board9_F6 (legalNextPosForPieceAtPos (Pawn White) board9 ('F', 6))
            -- Non starting position opposite colour pieces diagonally in front (board9)
        assertListsEqualAsSets board9_B6 (legalNextPosForPieceAtPos (Pawn White) board9 ('B', 6))

        -- Black:
            -- Non starting position no space in front
        assertListsEqualAsSets board10_D1 (legalNextPosForPieceAtPos (Pawn Black) board10 ('D', 1))
            -- Non starting position same colour pieces directly in front
        assertListsEqualAsSets board10_E2 (legalNextPosForPieceAtPos (Pawn Black) board10 ('E', 2))
            -- Non starting position same colour pieces diagonally in front
        assertListsEqualAsSets board10_B3 (legalNextPosForPieceAtPos (Pawn Black) board10 ('B', 3))
            -- Non starting position opposite colour pieces directly in front
        assertListsEqualAsSets board10_H4 (legalNextPosForPieceAtPos (Pawn Black) board10 ('H', 4))
            -- Non starting position opposite colour pieces diagonally in front
        assertListsEqualAsSets board10_F6 (legalNextPosForPieceAtPos (Pawn Black) board10 ('F', 6))
            -- Non starting position no pieces in front
        assertListsEqualAsSets board10_B6 (legalNextPosForPieceAtPos (Pawn Black) board10 ('B', 6))

test_legalNextPosForPieceAtPos_Queen = 
    do
        -- freshboard
        assertEqual [] (legalNextPosForPieceAtPos (Queen White) freshBoard ('D', 1))
        assertEqual [] (legalNextPosForPieceAtPos (Queen Black) freshBoard ('D', 8))

        assertListsEqualAsSets board2_C6 (legalNextPosForPieceAtPos (Queen Black) board2 ('C', 6))
        assertListsEqualAsSets board3_E6 (legalNextPosForPieceAtPos (Queen White) board3 ('E', 6))
        assertListsEqualAsSets board4_E3 (legalNextPosForPieceAtPos (Queen White) board4 ('E', 3))
        assertListsEqualAsSets board5_D6 (legalNextPosForPieceAtPos (Queen White) board5 ('D', 6))
 
test_getPieceAt = 
    do 
        assertEqual (Just (Rook White)) (getPieceAt ('A', 1) freshBoard)
        assertEqual (Just (King White)) (getPieceAt ('E', 1) freshBoard)
        assertEqual (Just (Pawn Black)) (getPieceAt ('G', 7) freshBoard)
        assertEqual (Nothing) (getPieceAt ('A', 5) freshBoard)
        assertEqual (Nothing) (getPieceAt ('E', 4) freshBoard)


main = htfMain htf_thisModulesTests