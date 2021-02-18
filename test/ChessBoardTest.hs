{-# OPTIONS_GHC -F -pgmF htfpp #-}

import Test.Framework
import ChessBoard
import ChessUtilTypes
import TestChessBoards

test_getPieceAt = 
    do 
        assertEqual (Just (Rook White)) (getPieceAt ('A', 1) freshBoard)
        assertEqual (Just (King White)) (getPieceAt ('E', 1) freshBoard)
        assertEqual (Just (Pawn Black)) (getPieceAt ('G', 7) freshBoard)
        assertEqual (Nothing) (getPieceAt ('A', 5) freshBoard)
        assertEqual (Nothing) (getPieceAt ('E', 4) freshBoard)

test_gameOver = 
    do
        assertEqual False (gameOver (ChessBoard [] Ongoing))
        assertEqual True (gameOver (ChessBoard [] Over))

test_filter = 
    do
        assertEqual (ChessBoard [ (('A', 1), Rook White) ] Ongoing) (filterChessBoard board1 White)
        assertEqual (ChessBoard [] Ongoing) (filterChessBoard board1 Black)
        assertEqual (ChessBoard [ (('A', 2), Rook White)
                                , (('E', 1), Bishop White)
                                , (('H', 1), Knight White)] Ongoing) (filterChessBoard board2 White)
        assertEqual (ChessBoard [ (('F', 6), Pawn Black)
                                , (('C', 6), Queen Black)
                                , (('D', 8), King Black) ] Ongoing) (filterChessBoard board2 Black)

main = htfMain htf_thisModulesTests