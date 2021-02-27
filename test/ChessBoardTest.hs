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
                                , (('H', 1), Knight White)] Over) (filterChessBoard board2 White)
        assertEqual (ChessBoard [ (('F', 6), Pawn Black)
                                , (('C', 6), Queen Black)
                                , (('D', 8), King Black) ] Over) (filterChessBoard board2 Black)


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
        assertEqual (fenToChessBoardWithState "3k4/8/5p2/2q5/8/8/R7/4B2N" Over)             (makeMove board2 (ChessMove ('C', 6) ('C', 5)))
        assertEqual (fenToChessBoardWithState "3k4/8/4qp2/8/8/8/R7/4B2N" Over)              (makeMove board2 (ChessMove ('C', 6) ('E', 6)))
        assertEqual (fenToChessBoardWithState "3k4/8/5p2/8/8/8/q7/4B2N" Over)               (makeMove board2 (ChessMove ('C', 6) ('A', 2)))
        assertEqual (fenToChessBoardWithState "3q4/8/5p2/8/8/8/R7/4B2N" Over)               (makeMove board2 (ChessMove ('C', 6) ('D', 8)))
        assertEqual (fenToChessBoardWithState "3k4/8/5p2/8/8/8/R7/4B2q" Over)               (makeMove board2 (ChessMove ('C', 6) ('H', 1)))
        assertEqual (fenToChessBoardWithState "8/p3p2p/1R1Q2r1/8/8/1p1b1n2/P3P2P/8" Over)   (makeMove board5 (ChessMove ('G', 3) ('G', 6)))
        assertEqual (fenToChessBoardWithState "8/p3p2p/3Q2N1/8/8/1R1b1nr1/P3P2P/8" Over)    (makeMove board5 (ChessMove ('B', 6) ('B', 3)))
        assertEqual (fenToChessBoardWithState "K7/8/3k4/4P3/8/8/8/8" Ongoing)               (makeMove (fenToChessBoardWithState "K7/8/3k4/8/4P3/8/8/8" Ongoing) (ChessMove ('E', 4) ('E', 5)))
        assertEqual (fenToChessBoardWithState "3P4/8/3PK3/8/8/8/8/8" Over)                (makeMove (fenToChessBoardWithState "3k4/4P3/3PK3/8/8/8/8/8" Ongoing) (ChessMove ('E', 7) ('D', 8)))


test_kingDead = 
    do
        assertEqual True (kingDead (fenToChessBoard "8/4P3/3PK3/8/8/8/8/8") Black)
        assertEqual False (kingDead (fenToChessBoard "8/4P3/3PK3/8/8/8/8/8") White)
        assertEqual True (kingDead (fenToChessBoard "k7/4P3/3P4/8/8/8/8/8") White)
        assertEqual False (kingDead (fenToChessBoard "k7/4P3/3P4/8/8/8/8/8") Black)


test_legalMoves = 
    do
        assertListsEqualAsSets [(ChessMove ('A', 2) ('A', 3))
                               ,(ChessMove ('A', 2) ('A', 4))
                               ,(ChessMove ('B', 2) ('B', 3))
                               ,(ChessMove ('B', 2) ('B', 4))
                               ,(ChessMove ('C', 2) ('C', 3))
                               ,(ChessMove ('C', 2) ('C', 4))
                               ,(ChessMove ('D', 2) ('D', 3))
                               ,(ChessMove ('D', 2) ('D', 4))
                               ,(ChessMove ('E', 2) ('E', 3))
                               ,(ChessMove ('E', 2) ('E', 4))
                               ,(ChessMove ('F', 2) ('F', 3))
                               ,(ChessMove ('F', 2) ('F', 4))
                               ,(ChessMove ('G', 2) ('G', 3))
                               ,(ChessMove ('G', 2) ('G', 4))
                               ,(ChessMove ('H', 2) ('H', 3))
                               ,(ChessMove ('H', 2) ('H', 4))
                               ,(ChessMove ('B', 1) ('A', 3))
                               ,(ChessMove ('B', 1) ('C', 3))
                               ,(ChessMove ('G', 1) ('F', 3))
                               ,(ChessMove ('G', 1) ('H', 3))] (legalMoves freshBoard White)
        assertListsEqualAsSets [(ChessMove ('A', 7) ('A', 6))
                               ,(ChessMove ('A', 7) ('A', 5))
                               ,(ChessMove ('B', 7) ('B', 6))
                               ,(ChessMove ('B', 7) ('B', 5))
                               ,(ChessMove ('C', 7) ('C', 6))
                               ,(ChessMove ('C', 7) ('C', 5))
                               ,(ChessMove ('D', 7) ('D', 6))
                               ,(ChessMove ('D', 7) ('D', 5))
                               ,(ChessMove ('E', 7) ('E', 6))
                               ,(ChessMove ('E', 7) ('E', 5))
                               ,(ChessMove ('F', 7) ('F', 6))
                               ,(ChessMove ('F', 7) ('F', 5))
                               ,(ChessMove ('G', 7) ('G', 6))
                               ,(ChessMove ('G', 7) ('G', 5))
                               ,(ChessMove ('H', 7) ('H', 6))
                               ,(ChessMove ('H', 7) ('H', 5))
                               ,(ChessMove ('B', 8) ('A', 6))
                               ,(ChessMove ('B', 8) ('C', 6))
                               ,(ChessMove ('G', 8) ('F', 6))
                               ,(ChessMove ('G', 8) ('H', 6))] (legalMoves freshBoard Black)
        
main = htfMain htf_thisModulesTests