{-# OPTIONS_GHC -F -pgmF htfpp #-}

import Test.Framework
import ChessUtilTypes
import ChessBoard
import FENotation

test_fenToChessBoard =
    do
        assertEqual (ChessBoard [] Ongoing) (fenToChessBoard "8/8/8/8/8/8/8/8")
        assertEqual (ChessBoard [ (('A', 1), Rook White) ] Ongoing) (fenToChessBoard "8/8/8/8/8/8/8/R7")
        assertEqual (ChessBoard [ (('A', 2), Rook White)
                                , (('E', 1), Bishop White)
                                , (('H', 1), Knight White)
                                , (('F', 6), Pawn Black)
                                , (('C', 6), Queen Black)
                                , (('D', 8), King Black) ] Ongoing) (fenToChessBoard "3k4/8/2q2p2/8/8/8/R7/4B2N")
        assertEqual (ChessBoard [ (('A', 2), Pawn White) 
                                , (('E', 2), Pawn White) 
                                , (('H', 2), Pawn White) 
                                , (('A', 3), Pawn Black) 
                                , (('E', 3), Bishop Black) 
                                , (('H', 3), Rook Black) 
                                , (('A', 6), Rook White) 
                                , (('E', 6), Queen White) 
                                , (('H', 6), Knight White)
                                , (('A', 7), Pawn Black) 
                                , (('E', 7), Pawn Black) 
                                , (('H', 7), Pawn Black) ] Ongoing) (fenToChessBoard "8/p3p2p/R3Q2N/8/8/p3b2r/P3P2P/8")
        assertEqual (ChessBoard [ (('A', 2), Pawn White) 
                                , (('E', 2), Pawn White) 
                                , (('H', 2), Pawn White) 
                                , (('A', 6), Pawn Black) 
                                , (('E', 6), Bishop Black) 
                                , (('H', 6), Rook Black) 
                                , (('A', 3), Rook White) 
                                , (('E', 3), Queen White) 
                                , (('H', 3), Knight White)
                                , (('A', 7), Pawn Black) 
                                , (('E', 7), Pawn Black) 
                                , (('H', 7), Pawn Black) ] Ongoing) (fenToChessBoard "8/p3p2p/p3b2r/8/8/R3Q2N/P3P2P/8")
        assertEqual (ChessBoard [ (('D', 1), Pawn Black) 
                                , (('E', 2), Pawn Black) 
                                , (('B', 2), Bishop Black) 
                                , (('B', 3), Pawn Black) 
                                , (('G', 3), Rook White) 
                                , (('H', 4), Pawn Black) 
                                , (('F', 5), Knight White) 
                                , (('F', 6), Pawn Black) 
                                , (('B', 6), Pawn Black) ] Ongoing) (fenToChessBoard "8/8/1p3p2/5N2/7p/1p4R1/1b2p3/3p4")

main = htfMain htf_thisModulesTests