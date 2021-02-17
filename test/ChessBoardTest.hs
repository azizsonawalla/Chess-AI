{-# OPTIONS_GHC -F -pgmF htfpp #-}

import Test.Framework
import ChessBoard
import ChessUtilTypes

-- Sample Chess Boards
-- freshboard = https://lichess.org/editor/rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR_w_KQkq_-_0_1

-- https://lichess.org/editor/8/8/8/8/8/8/8/R7_w_-_-_0_1
board1 = ChessBoard [ (('A', 1), Rook White) ] Ongoing

-- https://lichess.org/editor/3k4/8/2q2p2/8/8/8/R7/4B2N_w_-_-_0_1
board2 = ChessBoard 
    [ (('A', 2), Rook White)
    , (('E', 1), Bishop White)
    , (('H', 1), Knight White)
    , (('F', 6), Pawn Black False)
    , (('C', 6), Queen Black)
    , (('D', 8), King Black) ] Ongoing

test_getPieceAt = 
    do 
        assertEqual (Just (Rook White)) (getPieceAt ('A', 1) freshBoard)
        assertEqual (Just (King White)) (getPieceAt ('E', 1) freshBoard)
        assertEqual (Just (Pawn Black True)) (getPieceAt ('G', 7) freshBoard)
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
        assertEqual (ChessBoard [ (('F', 6), Pawn Black False)
                                , (('C', 6), Queen Black)
                                , (('D', 8), King Black) ] Ongoing) (filterChessBoard board2 Black)

main = htfMain htf_thisModulesTests