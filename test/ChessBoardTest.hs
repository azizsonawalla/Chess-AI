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
        freshBoard <- readFile "test/resources/freshboard_string.txt" 
        assertEqual freshBoard (chessBoardAsString (fenToChessBoard "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR"))
        -- random board 1
        randomBoard <- readFile "test/resources/randomboard_string.txt" 
        assertEqual randomBoard (chessBoardAsString (fenToChessBoard "k7/q1P2BN1/1P1p3b/P6r/5pp1/4p2r/1K6/1R6"))
        -- random board 2
        randomBoard2 <- readFile "test/resources/randomboard2_string.txt" 
        assertEqual randomBoard2 (chessBoardAsString (fenToChessBoard "8/K4P2/p5b1/p2k2nq/1P1p3B/R1pp4/N2PP2Q/1n6"))
        -- empty board 
        randomBoard2 <- readFile "test/resources/emptyboard_string.txt" 
        assertEqual randomBoard2 (chessBoardAsString (fenToChessBoard "8/8/8/8/8/8/8/8"))

test_makeMove = -- makeMove :: ChessBoard -> ChessMove -> ChessBoard
    do
        assertEqual (fenToChessBoard "3k4/8/5p2/2q5/8/8/R7/4B2N") (makeMove board2 (ChessMove ('C', 6) ('C', 5)))
        assertEqual (fenToChessBoard "3k4/8/4qp2/8/8/8/R7/4B2N")  (makeMove board2 (ChessMove ('C', 6) ('E', 6)))
        assertEqual (fenToChessBoard "3k4/8/5p2/8/8/8/q7/4B2N")   (makeMove board2 (ChessMove ('C', 6) ('A', 2)))
        assertEqual (fenToChessBoard "3q4/8/5p2/8/8/8/R7/4B2N")   (makeMove board2 (ChessMove ('C', 6) ('D', 8)))
        assertEqual (fenToChessBoard "3k4/8/5p2/8/8/8/R7/4B2q")   (makeMove board2 (ChessMove ('C', 6) ('H', 1)))
        assertEqual (fenToChessBoard "8/p3p2p/1R1Q2r1/8/8/1p1b1n2/P3P2P/8")   (makeMove board5 (ChessMove ('G', 3) ('G', 6)))
        assertEqual (fenToChessBoard "8/p3p2p/3Q2N1/8/8/1R1b1nr1/P3P2P/8")   (makeMove board5 (ChessMove ('B', 6) ('B', 3)))

test_validMove = 
    do
        assertEqual True (validMove freshBoard White (ChessMove ('A', 2) ('A', 3))) 
        assertEqual True (validMove freshBoard White (ChessMove ('A', 2) ('A', 4)))
        assertEqual True (validMove freshBoard White (ChessMove ('B', 2) ('B', 3))) 
        assertEqual True (validMove freshBoard White (ChessMove ('B', 2) ('B', 4)))
        assertEqual True (validMove freshBoard White (ChessMove ('A', 2) ('A', 3))) 
        assertEqual True (validMove freshBoard White (ChessMove ('C', 2) ('C', 3)))
        assertEqual True (validMove freshBoard White (ChessMove ('C', 2) ('C', 4)))
        assertEqual False (validMove freshBoard White (ChessMove ('G', 1) ('H', 6)))
        assertEqual False (validMove freshBoard White (ChessMove ('B', 8) ('C', 6)))
        assertEqual False (validMove freshBoard White (ChessMove ('H', 7) ('H', 6)))

        assertEqual True (validMove freshBoard Black (ChessMove ('A', 7) ('A', 6)))
        assertEqual True (validMove freshBoard Black (ChessMove ('A', 7) ('A', 5)))
        assertEqual True (validMove freshBoard Black (ChessMove ('B', 7) ('B', 6)))
        assertEqual True (validMove freshBoard Black (ChessMove ('B', 7) ('B', 5)))
        assertEqual True (validMove freshBoard Black (ChessMove ('C', 7) ('C', 6)))
        assertEqual True (validMove freshBoard Black (ChessMove ('C', 7) ('C', 5)))
        assertEqual True (validMove freshBoard Black (ChessMove ('D', 7) ('D', 6)))
        assertEqual False (validMove freshBoard Black (ChessMove ('A', 1) ('H', 6)))
        assertEqual False (validMove freshBoard Black (ChessMove ('B', 2) ('C', 1)))
        assertEqual False (validMove freshBoard Black (ChessMove ('H', 1) ('H', 2)))

main = htfMain htf_thisModulesTests