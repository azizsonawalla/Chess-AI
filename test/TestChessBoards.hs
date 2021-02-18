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

-- https://lichess.org/editor/8/8/8/8/8/8/8/R7_w_-_-_0_1
board1 = fenToChessBoard "8/8/8/8/8/8/8/R7"
board1_A1 = [ ('A', 2) , ('A', 3) , ('A', 4) , ('A', 5) , ('A', 6) , ('A', 7) , ('A', 8) , ('B', 1) , ('C', 1) , ('D', 1) , ('E', 1) , ('F', 1) , ('G', 1) , ('H', 1) ]


-- https://lichess.org/editor/3k4/8/2q2p2/8/8/8/R7/4B2N_w_-_-_0_1
board2 = fenToChessBoard "3k4/8/2q2p2/8/8/8/R7/4B2N"

-- https://lichess.org/editor/8/p3p2p/R3Q2N/8/8/p3b2r/P3P2P/8_w_-_-_0_1
board3 = fenToChessBoard "8/p3p2p/R3Q2N/8/8/p3b2r/P3P2P/8"

-- https://lichess.org/editor/8/p3p2p/p3b2r/8/8/R3Q2N/P3P2P/8_w_-_-_0_1
board4 = fenToChessBoard "8/p3p2p/p3b2r/8/8/R3Q2N/P3P2P/8"

-- https://lichess.org/editor/8/p3p2p/1R1Q2N1/8/8/1p1b2r1/P3P2P/8_w_-_-_0_1
board5 = fenToChessBoard "8/p3p2p/1R1Q2N1/8/8/1p1b2r1/P3P2P/8"

-- https://lichess.org/editor/8/p3p2p/1p1b2r1/8/8/1R1Q2N1/P3P2P/8_w_-_-_0_1
board6 = fenToChessBoard "8/p3p2p/1p1b2r1/8/8/1R1Q2N1/P3P2P/8"

-- https://lichess.org/editor/8/p3p2p/8/R3Q2N/p3b2r/8/P3P2P/8_w_-_-_0_1
board7 = fenToChessBoard "8/p3p2p/8/R3Q2N/p3b2r/8/P3P2P/8"

-- https://lichess.org/editor/8/p3p2p/8/p3b2r/R3Q2N/8/P3P2P/8_w_-_-_0_1
board8 = fenToChessBoard "8/p3p2p/8/p3b2r/R3Q2N/8/P3P2P/8"

-- https://lichess.org/editor/8/Q4b2/1P3P2/3R4/3P3n/P5P1/8/8_w_-_-_0_1
board9 = fenToChessBoard "8/Q4b2/1P3P2/3R4/3P3n/P5P1/8/8"

-- https://lichess.org/editor/8/8/1p3p2/5N2/7p/1p4R1/1b2p3/3p4_w_-_-_0_1
board10 = fenToChessBoard "8/8/1p3p2/5N2/7p/1p4R1/1b2p3/3p4"