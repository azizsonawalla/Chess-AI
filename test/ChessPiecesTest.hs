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


test_legalNextPosForPieceAtPos_Pawn = 
    do
        -- A2 pawn at starting position (freshboard)
        assertEqual freshBoard_A2 (legalNextPosForPieceAtPos (Pawn White) freshBoard ('A', 2))
        -- A2 pawn at starting position with opposite piece in front (board3)
        -- A2 pawn at starting position with same colour piece in front (board4)
        -- A2 pawn at starting position with opposite piece diagonally in front (board5)
        -- A2 pawn at starting position with same colour piece diagonally in front (board6)
        -- A2 pawn at starting position with opposite piece one square away (board7)
        -- A2 pawn at starting position with same colour piece one square away (board8)

        -- E2 pawn at starting position (freshboard)
        assertEqual freshBoard_E2 (legalNextPosForPieceAtPos (Pawn White) freshBoard ('E', 2))
        -- E2 pawn at starting position with opposite piece in front (board3)
        -- E2 pawn at starting position with same colour piece in front (board4)
        -- E2 pawn at starting position with opposite piece diagonally in front (board5)
        -- E2 pawn at starting position with same colour piece diagonally in front (board6)
        -- E2 pawn at starting position with opposite piece one square away (board7)
        -- E2 pawn at starting position with same colour piece one square away (board8)

        -- H2 pawn at starting position (freshboard)
        assertEqual freshBoard_H2 (legalNextPosForPieceAtPos (Pawn White) freshBoard ('H', 2))
        -- H2 pawn at starting position with opposite piece in front (board3)
        -- H2 pawn at starting position with same colour piece in front (board4)
        -- H2 pawn at starting position with opposite piece diagonally in front (board5)
        -- H2 pawn at starting position with same colour piece diagonally in front (board6)
        -- H2 pawn at starting position with opposite piece one square away (board7)
        -- H2 pawn at starting position with same colour piece one square away (board8)
        
        -- A7 pawn at starting position (freshboard)
        assertEqual freshBoard_A7 (legalNextPosForPieceAtPos (Pawn Black) freshBoard ('A', 7))
        -- A7 pawn at starting position with opposite piece in front (board3)
        -- A7 pawn at starting position with same colour piece in front (board4)
        -- A7 pawn at starting position with opposite piece diagonally in front (board5)
        -- A7 pawn at starting position with same colour piece diagonally in front (board6)
        -- A7 pawn at starting position with opposite piece one square away (board7)
        -- A7 pawn at starting position with same colour piece one square away (board8)

        -- E7 pawn at starting position (freshboard)
        assertEqual freshBoard_E7 (legalNextPosForPieceAtPos (Pawn Black) freshBoard ('E', 7))
        -- E7 pawn at starting position with opposite piece in front (board3)
        -- E7 pawn at starting position with same colour piece in front (board4)
        -- E7 pawn at starting position with opposite piece diagonally in front (board5)
        -- E7 pawn at starting position with same colour piece diagonally in front (board6)
        -- E7 pawn at starting position with opposite piece one square away (board7)
        -- E7 pawn at starting position with same colour piece one square away (board8)

        -- H7 pawn at starting position (freshboard)
        assertEqual freshBoard_H7 (legalNextPosForPieceAtPos (Pawn Black) freshBoard ('H', 7))
        -- H7 pawn at starting position with opposite piece in front (board3)
        -- H7 pawn at starting position with same colour piece in front (board4)
        -- H7 pawn at starting position with opposite piece diagonally in front (board5)
        -- H7 pawn at starting position with same colour piece diagonally in front (board6)
        -- H7 pawn at starting position with opposite piece one square away (board7)
        -- H7 pawn at starting position with same colour piece one square away (board8)

        -- White:
            -- Non starting position no pieces in front (board9)
            -- Non starting position same colour pieces directly in front (board9)
            -- Non starting position same colour pieces diagonally in front (board9)
            -- Non starting position opposite colour pieces directly in front (board9)
            -- Non starting position opposite colour pieces diagonally in front (board9)

        -- Black:
            -- Non starting position no pieces in front
            -- Non starting position same colour pieces directly in front
            -- Non starting position same colour pieces diagonally in front
            -- Non starting position opposite colour pieces directly in front
            -- Non starting position opposite colour pieces diagonally in front


main = htfMain htf_thisModulesTests