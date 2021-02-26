module TestChessBoards where

import ChessBoard
import ChessUtilTypes
import FENotation

-- Possible moves for pieces on the freshBoard
freshBoard_A2 = [('A', 3), ('A', 4)] -- positions that the piece at A2 can move to
freshBoard_E2 = [('E', 3), ('E', 4)]
freshBoard_H2 = [('H', 3), ('H', 4)]
freshBoard_A7 = [('A', 6), ('A', 5)]
freshBoard_E7 = [('E', 6), ('E', 5)]
freshBoard_H7 = [('H', 6), ('H', 5)]
freshBoard_C1 = []
freshBoard_F1 = []
freshBoard_C8 = []
freshBoard_F8 = []

-- https://lichess.org/editor/8/8/8/8/8/8/8/R7_w_-_-_0_1
board1 = fenToChessBoard "8/8/8/8/8/8/8/R7"
board1_A1 = [ ('A', 2) , ('A', 3) , ('A', 4) , ('A', 5) , ('A', 6) , ('A', 7) , ('A', 8) , ('B', 1) , ('C', 1) , ('D', 1) , ('E', 1) , ('F', 1) , ('G', 1) , ('H', 1) ]


-- https://lichess.org/editor/3k4/8/2q2p2/8/8/8/R7/4B2N_w_-_-_0_1
board2 = fenToChessBoard "3k4/8/2q2p2/8/8/8/R7/4B2N"
board2_E1 = [('F', 2), ('G', 3), ('H', 4), ('D', 2), ('C', 3), ('B', 4), ('A', 5)]
board2_D8 = [('D', 7), ('C', 8), ('E', 8), ('C', 7), ('E', 7)]
board2_A2 = [('A', 3), ('A', 4), ('A', 5), ('A', 6), ('A', 7), ('A', 8), ('A', 1), ('B', 2), ('C', 2), ('D', 2), ('E', 2), ('F', 2), ('G', 2), ('H', 2)]
board2_C6 = [('A', 4), ('A', 6), ('A', 8), ('B', 5), ('B', 6), ('B', 7), ('C', 1), ('C', 2), ('C', 3), ('C', 4), ('C', 5), ('C', 7), ('C', 8), ('D', 5), ('D', 6), ('D', 7), ('E', 4), ('E', 6), ('E', 8), ('F', 3), ('G', 2), ('H', 1)]

-- https://lichess.org/editor/8/p3p2p/R3Q2N/8/8/p3b2r/P3P2P/8_w_-_-_0_1
board3 = fenToChessBoard "8/p3p2p/R3Q2N/8/8/p3b2r/P3P2P/8"
board3_A2 = []
board3_E2 = []
board3_H2 = []
board3_A7 = []
board3_E7 = []
board3_H7 = []
board3_E3 = [('D', 2), ('C', 1), ('F', 2), ('G', 1), ('D', 4), ('C', 5), ('B', 6), ('F', 4), ('G', 5), ('H', 6)]
board3_H3 = [('H', 4), ('H', 5), ('H', 6), ('H', 2), ('G', 3), ('F', 3)]
board3_A6 = [('A', 7), ('A', 5), ('A', 4), ('A', 3), ('B', 6), ('C', 6), ('D', 6)]
board3_E6 = [('B', 3), ('B', 6), ('C', 4), ('C', 6), ('C', 8), ('D', 5), ('D', 6), ('D', 7), ('E', 3), ('E', 4), ('E', 5), ('E', 7), ('F', 5), ('F', 6), ('F', 7), ('G', 4), ('G', 6), ('G', 8), ('H', 3)]

-- https://lichess.org/editor/8/p3p2p/p3b2r/8/8/R3Q2N/P3P2P/8_w_-_-_0_1
board4 = fenToChessBoard "8/p3p2p/p3b2r/8/8/R3Q2N/P3P2P/8"
board4_A2 = []
board4_E2 = []
board4_H2 = []
board4_A7 = []
board4_E7 = []
board4_H7 = []
board4_E3 = [('A', 7), ('B', 3), ('B', 6), ('C', 1), ('C', 3), ('C', 5), ('D', 2), ('D', 3), ('D', 4), ('E', 4), ('E', 5), ('E', 6), ('F', 2), ('F', 3), ('F', 4), ('G', 1), ('G', 3), ('G', 5), ('H', 6)]


-- https://lichess.org/editor/8/p3p2p/1R1Q2N1/8/8/1p1b1nr1/P3P2P/8_w_-_-_0_1
board5 = fenToChessBoard "8/p3p2p/1R1Q2N1/8/8/1p1b1nr1/P3P2P/8"
board5_A2 = [('A', 3), ('A', 4), ('B', 3)]
board5_E2 = [('E', 3), ('E', 4), ('D', 3), ('F', 3)]
board5_H2 = [('H', 3), ('H', 4), ('G', 3)]
board5_A7 = [('A', 6), ('A', 5), ('B', 6)]
board5_E7 = [('E', 6), ('E', 5), ('D', 6)]
board5_H7 = [('H', 6), ('H', 5), ('G', 6)]
board5_D6 = [('A', 3), ('B', 4), ('B', 8), ('C', 5), ('C', 6), ('C', 7), ('D', 3), ('D', 4), ('D', 5), ('D', 7), ('D', 8), ('E', 5), ('E', 6), ('E', 7), ('F', 4), ('F', 6), ('G', 3)]

-- https://lichess.org/editor/8/p3p2p/1p1b2r1/8/8/1R1Q2N1/P3P2P/8_w_-_-_0_1
board6 = fenToChessBoard "8/p3p2p/1p1b2r1/8/8/1R1Q2N1/P3P2P/8"
board6_A2 = [('A', 3), ('A', 4)]
board6_E2 = [('E', 3), ('E', 4)]
board6_H2 = [('H', 3), ('H', 4)]
board6_A7 = [('A', 6), ('A', 5)]
board6_E7 = [('E', 6), ('E', 5)]
board6_H7 = [('H', 6), ('H', 5)]

-- https://lichess.org/editor/8/p3p2p/8/R3Q2N/p3b2r/8/P3P2P/8_w_-_-_0_1
board7 = fenToChessBoard "8/p3p2p/8/R3Q2N/p3b2r/8/P3P2P/8"
board7_A2 = [('A', 3)]
board7_E2 = [('E', 3)]
board7_H2 = [('H', 3)]
board7_A7 = [('A', 6)]
board7_E7 = [('E', 6)]
board7_H7 = [('H', 6)]

-- https://lichess.org/editor/8/p3p2p/8/p3b2r/R3Q2N/8/P3P2P/8_w_-_-_0_1
board8 = fenToChessBoard "8/p3p2p/8/p3b2r/R3Q2N/8/P3P2P/8"
board8_A2 = [('A', 3)]
board8_E2 = [('E', 3)]
board8_H2 = [('H', 3)]
board8_A7 = [('A', 6)]
board8_E7 = [('E', 6)]
board8_H7 = [('H', 6)]

-- https://lichess.org/editor/8/Q4b2/1P3P2/3R4/3P3n/P5P1/8/8_w_-_-_0_1
board9 = fenToChessBoard "8/Q4b2/1P3P2/3R4/3P3n/P5P1/8/8"
board9_A3 = [('A', 4)]
board9_G3 = [('G', 4), ('H', 4)]
board9_D4 = []
board9_F6 = []
board9_B6 = [('B', 7)]

-- https://lichess.org/editor/8/8/1p3p2/5N2/7p/1p4R1/1b2p3/3p4_w_-_-_0_1
board10 = fenToChessBoard "8/8/1p3p2/5N2/7p/1p4R1/1b2p3/3p4"
board10_D1 = []
board10_E2 = [('E', 1)]
board10_B3 = []
board10_H4 = [('H', 3), ('G', 3)]
board10_F6 = []
board10_B6 = [('B', 5)]

-- https://lichess.org/editor/5b2/b1B5/3b3B/8/1B3B2/B7/3B3b/8_w_-_-_0_1
board11 = fenToChessBoard "5b2/b1B5/3b3B/8/1B3B2/B7/3B3b/8"
board11_D2 = [('C', 3), ('E', 3), ('C', 1), ('E', 1)]
board11_H2 = [('G', 1), ('G', 3), ('F', 4)]
board11_A3 = [('B', 2), ('C', 1)]
board11_B4 = [('C', 3), ('D', 6), ('C', 5), ('A', 5)]
board11_F4 = [('E', 3), ('E', 5), ('D', 6), ('G', 3), ('H', 2), ('G', 5)]
board11_D6 = [('E', 5), ('F', 4), ('C', 5), ('B', 4), ('C', 7), ('E', 7)]
board11_H6 = [('G', 5), ('G', 7), ('F', 8)]
board11_A7 = [('B', 8), ('B', 6), ('C', 5), ('D', 4), ('E', 3), ('F', 2), ('G', 1)]
board11_C7 = [('B', 8), ('B', 6), ('A', 5), ('D', 6), ('D', 8)]
board11_F8 = [('E', 7), ('G', 7), ('H', 6)]
 
-- https://lichess.org/editor/8/P7/N2pB2p/1N2k1Pp/R3q1p1/P4rnP/2b1p3/K7_w_-_-_0_1
board12 = fenToChessBoard "8/P7/N2pB2p/1N2k1Pp/R3q1p1/P4rnP/2b1p3/K7"
board12_A1 = [('A', 2), ('B', 1), ('B', 2)]
board12_E5 = [('E', 6), ('D', 5), ('F', 5), ('F', 6), ('D', 4), ('F', 4)]


-- https://lichess.org/editor/k3B3/2Q5/2r1pP2/P2pP1p1/Pb6/1P1PN2p/1R5P/K7_w_-_-_0_1
board13 = fenToChessBoard "k3B3/2Q5/2r1pP2/P2pP1p1/Pb6/1P1PN2p/1R5P/K7"
board13_A1 = [('A', 2), ('B', 1)] -- white king
board13_A8 = [('A', 7), ('B', 8), ('B', 7)] -- black king
board13_B2 = [('B', 1), ('A', 2), ('C', 2), ('D', 2), ('E', 2), ('F', 2), ('G', 2)] -- white rook
board13_C6 = [('C', 7), ('C', 5), ('C', 4), ('C', 3), ('C', 2), ('C', 1), ('B', 6), ('A', 6), ('D', 6)] -- black rook
board13_F8 = [('E', 7), ('G', 7), ('H', 6)]

-- https://lichess.org/editor/N5rN/2b3br/1N4Q1/3N4/8/1R3N2/2Q5/N3Kr1N_w_-_-_0_1
board14 = fenToChessBoard "N5rN/2b3br/1N4Q1/3N4/8/1R3N2/2Q5/N3Kr1N"
board14_A1 = []
board14_H1 = [('F', 2), ('G', 3)]
board14_F3 = [('G', 1), ('D', 2), ('H', 2), ('D', 4), ('H', 4), ('E', 5), ('G', 5)]
board14_D5 = [('C', 3), ('E', 3), ('B', 4), ('F', 4), ('C', 7), ('E', 7), ('F', 6)]
board14_B6 = [('A', 4), ('C', 4), ('D', 7), ('C', 8)]
board14_H8 = [('F', 7)]
board14_A8 = [('C', 7)]

-- https://lichess.org/editor/n5Rn/2B3BR/1n4q1/3n4/8/1r3n2/2q5/n3kR1n_w_-_-_0_1
board15 = fenToChessBoard "n5Rn/2B3BR/1n4q1/3n4/8/1r3n2/2q5/n3kR1n"
board15_A1 = []
board15_H1 = [('F', 2), ('G', 3)]
board15_F3 = [('G', 1), ('D', 2), ('H', 2), ('D', 4), ('H', 4), ('E', 5), ('G', 5)]
board15_D5 = [('C', 3), ('E', 3), ('B', 4), ('F', 4), ('C', 7), ('E', 7), ('F', 6)]
board15_B6 = [('A', 4), ('C', 4), ('D', 7), ('C', 8)]
board15_H8 = [('F', 7)]
board15_A8 = [('C', 7)]

-- https://lichess.org/editor/8/8/3k4/4P3/8/8/8/8_w_-_-_0_1
board16 = fenToChessBoard "8/8/3k4/4P3/8/8/8/8"

board16_W1_Board = fenToChessBoard "8/8/3kP3/8/8/8/8/8"
board16_W1_Move = (ChessMove ('E', 5) ('E', 6))

board16_W2_Board = fenToChessBoard "8/8/3P4/8/8/8/8/8"
board16_W2_Move = (ChessMove ('E', 5) ('D', 6))

board16_B1_Board = fenToChessBoard "8/8/8/4k3/8/8/8/8"
board16_B1_Move = (ChessMove ('D', 6) ('E', 5))

board16_B2_Board = fenToChessBoard "8/8/8/3kP3/8/8/8/8"
board16_B2_Move = (ChessMove ('D', 6) ('D', 5))

board16_B3_Board = fenToChessBoard "8/8/8/2k1P3/8/8/8/8"
board16_B3_Move = (ChessMove ('D', 6) ('C', 5))

board16_B4_Board = fenToChessBoard "8/8/2k5/4P3/8/8/8/8"
board16_B4_Move = (ChessMove ('D', 6) ('C', 6))

board16_B5_Board = fenToChessBoard "8/2k5/8/4P3/8/8/8/8"
board16_B5_Move = (ChessMove ('D', 6) ('C', 7))

board16_B6_Board = fenToChessBoard "8/3k4/8/4P3/8/8/8/8"
board16_B6_Move = (ChessMove ('D', 6) ('D', 7))

board16_B7_Board = fenToChessBoard "8/4k3/8/4P3/8/8/8/8"
board16_B7_Move = (ChessMove ('D', 6) ('E', 7))

board16_B8_Board = fenToChessBoard "8/8/4k3/4P3/8/8/8/8"
board16_B8_Move = (ChessMove ('D', 6) ('E', 6))