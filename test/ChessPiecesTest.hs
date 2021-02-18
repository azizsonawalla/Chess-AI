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
        assertEqual board3_A2 (legalNextPosForPieceAtPos (Pawn White) board3 ('A', 2))
        -- A2 pawn at starting position with same colour piece in front (board4)
        assertEqual board4_A2 (legalNextPosForPieceAtPos (Pawn White) board4 ('A', 2))
        -- A2 pawn at starting position with opposite piece diagonally in front (board5)
        assertEqual board5_A2 (legalNextPosForPieceAtPos (Pawn White) board5 ('A', 2))
        -- A2 pawn at starting position with same colour piece diagonally in front (board6)
        assertEqual board6_A2 (legalNextPosForPieceAtPos (Pawn White) board6 ('A', 2))
        -- A2 pawn at starting position with opposite piece one square away (board7)
        assertEqual board7_A2 (legalNextPosForPieceAtPos (Pawn White) board7 ('A', 2))
        -- A2 pawn at starting position with same colour piece one square away (board8)
        assertEqual board8_A2 (legalNextPosForPieceAtPos (Pawn White) board8 ('A', 2))

        -- E2 pawn at starting position (freshboard)
        assertEqual freshBoard_E2 (legalNextPosForPieceAtPos (Pawn White) freshBoard ('E', 2))
        -- E2 pawn at starting position with opposite piece in front (board3)
        assertEqual board3_E2 (legalNextPosForPieceAtPos (Pawn White) board3 ('E', 2))
        -- E2 pawn at starting position with same colour piece in front (board4)
        assertEqual board4_E2 (legalNextPosForPieceAtPos (Pawn White) board4 ('E', 2))
        -- E2 pawn at starting position with opposite piece diagonally in front (board5)
        assertEqual board5_E2 (legalNextPosForPieceAtPos (Pawn White) board5 ('E', 2))
        -- E2 pawn at starting position with same colour piece diagonally in front (board6)
        assertEqual board6_E2 (legalNextPosForPieceAtPos (Pawn White) board6 ('E', 2))
        -- E2 pawn at starting position with opposite piece one square away (board7)
        assertEqual board7_E2 (legalNextPosForPieceAtPos (Pawn White) board7 ('E', 2))
        -- E2 pawn at starting position with same colour piece one square away (board8)
        assertEqual board8_E2 (legalNextPosForPieceAtPos (Pawn White) board8 ('E', 2))

        -- H2 pawn at starting position (freshboard)
        assertEqual freshBoard_H2 (legalNextPosForPieceAtPos (Pawn White) freshBoard ('H', 2))
        -- H2 pawn at starting position with opposite piece in front (board3)
        assertEqual board3_H2 (legalNextPosForPieceAtPos (Pawn White) board3 ('H', 2))
        -- H2 pawn at starting position with same colour piece in front (board4)
        assertEqual board4_H2 (legalNextPosForPieceAtPos (Pawn White) board4 ('H', 2))
        -- H2 pawn at starting position with opposite piece diagonally in front (board5)
        assertEqual board5_H2 (legalNextPosForPieceAtPos (Pawn White) board5 ('H', 2))
        -- H2 pawn at starting position with same colour piece diagonally in front (board6)
        assertEqual board6_H2 (legalNextPosForPieceAtPos (Pawn White) board6 ('H', 2))
        -- H2 pawn at starting position with opposite piece one square away (board7)
        assertEqual board7_H2 (legalNextPosForPieceAtPos (Pawn White) board7 ('H', 2))
        -- H2 pawn at starting position with same colour piece one square away (board8)
        assertEqual board8_H2 (legalNextPosForPieceAtPos (Pawn White) board8 ('H', 2))
        
        -- A7 pawn at starting position (freshboard)
        assertEqual freshBoard_A7 (legalNextPosForPieceAtPos (Pawn Black) freshBoard ('A', 7))
        -- A7 pawn at starting position with opposite piece in front (board3)
        assertEqual board3_A7 (legalNextPosForPieceAtPos (Pawn Black) board3 ('A', 7))
        -- A7 pawn at starting position with same colour piece in front (board4)
        assertEqual board4_A7 (legalNextPosForPieceAtPos (Pawn Black) board4 ('A', 7))
        -- A7 pawn at starting position with opposite piece diagonally in front (board5)
        assertEqual board5_A7 (legalNextPosForPieceAtPos (Pawn Black) board5 ('A', 7))
        -- A7 pawn at starting position with same colour piece diagonally in front (board6)
        assertEqual board6_A7 (legalNextPosForPieceAtPos (Pawn Black) board6 ('A', 7))
        -- A7 pawn at starting position with opposite piece one square away (board7)
        assertEqual board7_A7 (legalNextPosForPieceAtPos (Pawn Black) board7 ('A', 7))
        -- A7 pawn at starting position with same colour piece one square away (board8)
        assertEqual board8_A7 (legalNextPosForPieceAtPos (Pawn Black) board8 ('A', 7))

        -- E7 pawn at starting position (freshboard)
        assertEqual freshBoard_E7 (legalNextPosForPieceAtPos (Pawn Black) freshBoard ('E', 7))
        -- E7 pawn at starting position with opposite piece in front (board3)
        assertEqual board3_E7 (legalNextPosForPieceAtPos (Pawn Black) board3 ('E', 7))
        -- E7 pawn at starting position with same colour piece in front (board4)
        assertEqual board4_E7 (legalNextPosForPieceAtPos (Pawn Black) board4 ('E', 7))
        -- E7 pawn at starting position with opposite piece diagonally in front (board5)
        assertEqual board5_E7 (legalNextPosForPieceAtPos (Pawn Black) board5 ('E', 7))
        -- E7 pawn at starting position with same colour piece diagonally in front (board6)
        assertEqual board6_E7 (legalNextPosForPieceAtPos (Pawn Black) board6 ('E', 7))
        -- E7 pawn at starting position with opposite piece one square away (board7)
        assertEqual board7_E7 (legalNextPosForPieceAtPos (Pawn Black) board7 ('E', 7))
        -- E7 pawn at starting position with same colour piece one square away (board8)
        assertEqual board8_E7 (legalNextPosForPieceAtPos (Pawn Black) board8 ('E', 7))

        -- H7 pawn at starting position (freshboard)
        assertEqual freshBoard_H7 (legalNextPosForPieceAtPos (Pawn Black) freshBoard ('H', 7))
        -- H7 pawn at starting position with opposite piece in front (board3)
        assertEqual board3_H7 (legalNextPosForPieceAtPos (Pawn Black) board3 ('H', 7))
        -- H7 pawn at starting position with same colour piece in front (board4)
        assertEqual board4_H7 (legalNextPosForPieceAtPos (Pawn Black) board4 ('H', 7))
        -- H7 pawn at starting position with opposite piece diagonally in front (board5)
        assertEqual board5_H7 (legalNextPosForPieceAtPos (Pawn Black) board5 ('H', 7))
        -- H7 pawn at starting position with same colour piece diagonally in front (board6)
        assertEqual board6_H7 (legalNextPosForPieceAtPos (Pawn Black) board6 ('H', 7))
        -- H7 pawn at starting position with opposite piece one square away (board7)
        assertEqual board7_H7 (legalNextPosForPieceAtPos (Pawn Black) board7 ('H', 7))
        -- H7 pawn at starting position with same colour piece one square away (board8)
        assertEqual board8_H7 (legalNextPosForPieceAtPos (Pawn Black) board8 ('H', 7))

      --   -- White:
      --       -- Non starting position no pieces in front (board9)
      --   assertEqual board9_A3 (legalNextPosForPieceAtPos (Pawn White) board9 ('A', 3))
      --       -- Non starting position same colour pieces directly in front (board9)
      --   assertEqual board9_G3 (legalNextPosForPieceAtPos (Pawn White) board9 ('G', 3))
      --       -- Non starting position same colour pieces diagonally in front (board9)
      --   assertEqual board9_D4 (legalNextPosForPieceAtPos (Pawn White) board9 ('D', 4))
      --       -- Non starting position opposite colour pieces directly in front (board9)
      --   assertEqual board9_F6 (legalNextPosForPieceAtPos (Pawn White) board9 ('F', 6))
      --       -- Non starting position opposite colour pieces diagonally in front (board9)
      --   assertEqual board9_B6 (legalNextPosForPieceAtPos (Pawn White) board9 ('B', 6))

      --   -- Black:
      --       -- Non starting position no space in front
      --   assertEqual board10_D1 (legalNextPosForPieceAtPos (Pawn Black) board10 ('D', 1))
      --       -- Non starting position same colour pieces directly in front
      --   assertEqual board10_E2 (legalNextPosForPieceAtPos (Pawn Black) board10 ('E', 2))
      --       -- Non starting position same colour pieces diagonally in front
      --   assertEqual board10_B3 (legalNextPosForPieceAtPos (Pawn Black) board10 ('B', 3))
      --       -- Non starting position opposite colour pieces directly in front
      --   assertEqual board10_H4 (legalNextPosForPieceAtPos (Pawn Black) board10 ('H', 4))
      --       -- Non starting position opposite colour pieces diagonally in front
      --   assertEqual board10_F6 (legalNextPosForPieceAtPos (Pawn Black) board10 ('F', 6))
      --       -- Non starting position no pieces in front
      --   assertEqual board10_B6 (legalNextPosForPieceAtPos (Pawn Black) board10 ('B', 6))


test_getPieceAt = 
    do 
        assertEqual (Just (Rook White)) (getPieceAt ('A', 1) freshBoard)
        assertEqual (Just (King White)) (getPieceAt ('E', 1) freshBoard)
        assertEqual (Just (Pawn Black)) (getPieceAt ('G', 7) freshBoard)
        assertEqual (Nothing) (getPieceAt ('A', 5) freshBoard)
        assertEqual (Nothing) (getPieceAt ('E', 4) freshBoard)


main = htfMain htf_thisModulesTests