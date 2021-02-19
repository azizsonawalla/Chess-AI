{-# OPTIONS_GHC -F -pgmF htfpp #-}

import Test.Framework
import ChessBoard
import ChessUtilTypes
import TestChessBoards
import System.IO
import FENotation

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


test_chessBoardAsString = 
    do
        -- fresh board
        freshBoard <- readFile "freshboard_string.txt" 
        assertEqual freshBoard (chessBoardAsString (fenToChessBoard "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR"))
        -- random board
        randomBoard <- readFile "randomboard_string.txt" 
        assertEqual randomBoard (chessBoardAsString (fenToChessBoard "k7/q1P2BN1/1P1p3b/P6r/5pp1/4p2r/1K6/1R6"))

main = htfMain htf_thisModulesTests