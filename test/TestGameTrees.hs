module TestGameTrees where

import TestChessBoards
import ChessUtilTypes
import AIPlayer

-- GameTree starting with board16, of depth 0, where White goes first
gameTree16_depth0_White = GameTree board16 0 []

-- GameTree starting with board16, of depth 0, where Black goes first
gameTree16_depth0_Black = GameTree board16 0 []

-- GameTree starting with board16, of depth 1, where White goes first
gameTree16_depth1_White = GameTree board16 0 [ MoveSubtree board16_W1_Move (GameTree board16_W1_Board 0 [])
                                             , MoveSubtree board16_W2_Move (GameTree board16_W2_Board 0 [])]

-- GameTree starting with board16, of depth 1, where Black goes first
gameTree16_depth1_Black = GameTree board16 0 [ MoveSubtree board16_B1_Move (GameTree board16_B1_Board 0 [])
                                             , MoveSubtree board16_B2_Move (GameTree board16_B2_Board 0 [])
                                             , MoveSubtree board16_B3_Move (GameTree board16_B3_Board 0 [])
                                             , MoveSubtree board16_B4_Move (GameTree board16_B4_Board 0 [])
                                             , MoveSubtree board16_B5_Move (GameTree board16_B5_Board 0 [])
                                             , MoveSubtree board16_B6_Move (GameTree board16_B6_Board 0 [])
                                             , MoveSubtree board16_B7_Move (GameTree board16_B7_Board 0 [])
                                             , MoveSubtree board16_B8_Move (GameTree board16_B8_Board 0 [])]

-- GameTree starting with board16, of depth 1, where White goes first, with fake scores (maximized)
gameTree16_depth1_White_scores_max = GameTree board16 50 [ MoveSubtree board16_W1_Move (GameTree board16_W1_Board 50 [])
                                                         , MoveSubtree board16_W2_Move (GameTree board16_W2_Board (-1) [])]

-- GameTree starting with board16, of depth 1, where Black goes first, with fake scores (maximized)
gameTree16_depth1_Black_scores_max = GameTree board16 501 [ MoveSubtree board16_B1_Move (GameTree board16_B1_Board 1 [])
                                                            , MoveSubtree board16_B2_Move (GameTree board16_B2_Board (-32) [])
                                                            , MoveSubtree board16_B3_Move (GameTree board16_B3_Board 50 [])
                                                            , MoveSubtree board16_B4_Move (GameTree board16_B4_Board (-81) [])
                                                            , MoveSubtree board16_B5_Move (GameTree board16_B5_Board 200 [])
                                                            , MoveSubtree board16_B6_Move (GameTree board16_B6_Board 500 [])
                                                            , MoveSubtree board16_B7_Move (GameTree board16_B7_Board (-800) [])
                                                            , MoveSubtree board16_B8_Move (GameTree board16_B8_Board 501 [])]

-- GameTree starting with board16, of depth 1, where White goes first, with fake scores (minimized)
gameTree16_depth1_White_scores_min = GameTree board16 (-1) [ MoveSubtree board16_W1_Move (GameTree board16_W1_Board 50 [])
                                                           , MoveSubtree board16_W2_Move (GameTree board16_W2_Board (-1) [])]

-- GameTree starting with board16, of depth 1, where Black goes first, with fake scores (minimized)
gameTree16_depth1_Black_scores_min = GameTree board16 (-800) [ MoveSubtree board16_B1_Move (GameTree board16_B1_Board 1 [])
                                                            , MoveSubtree board16_B2_Move (GameTree board16_B2_Board (-32) [])
                                                            , MoveSubtree board16_B3_Move (GameTree board16_B3_Board 50 [])
                                                            , MoveSubtree board16_B4_Move (GameTree board16_B4_Board (-81) [])
                                                            , MoveSubtree board16_B5_Move (GameTree board16_B5_Board 200 [])
                                                            , MoveSubtree board16_B6_Move (GameTree board16_B6_Board 500 [])
                                                            , MoveSubtree board16_B7_Move (GameTree board16_B7_Board (-800) [])
                                                            , MoveSubtree board16_B8_Move (GameTree board16_B8_Board 501 [])]


-- GameTree starting with board17, of depth 1, where White goes first
gameTree17_depth1_White = GameTree board17 0 [ MoveSubtree board17_W1_Move (GameTree board17_W1_Board 0 [])
                                             , MoveSubtree board17_W2_Move (GameTree board17_W2_Board 0 [])
                                             , MoveSubtree board17_W3_Move (GameTree board17_W3_Board 0 [])
                                             , MoveSubtree board17_W4_Move (GameTree board17_W4_Board 0 [])
                                             , MoveSubtree board17_W5_Move (GameTree board17_W5_Board 0 [])]

-- GameTree starting with board17, of depth 1, where Black goes first
gameTree17_depth1_Black = GameTree board17 0 [ MoveSubtree board17_B1_Move (GameTree board17_B1_Board 0 [])
                                             , MoveSubtree board17_B2_Move (GameTree board17_B2_Board 0 [])
                                             , MoveSubtree board17_B3_Move (GameTree board17_B3_Board 0 [])
                                             , MoveSubtree board17_B4_Move (GameTree board17_B4_Board 0 [])]

-- GameTree starting with board17, of depth 1, where White goes first, with fake scores (maximized)
gameTree17_depth1_White_scores_max = GameTree board17 50 [ MoveSubtree board17_W1_Move (GameTree board17_W1_Board 50 [])
                                                         , MoveSubtree board17_W2_Move (GameTree board17_W2_Board (-1) [])
                                                         , MoveSubtree board17_W3_Move (GameTree board17_W3_Board (-1) [])
                                                         , MoveSubtree board17_W4_Move (GameTree board17_W4_Board (-1) [])
                                                         , MoveSubtree board17_W5_Move (GameTree board17_W5_Board 50 [])]

-- GameTree starting with board17, of depth 1, where Black goes first, with fake scores (maximized)
gameTree17_depth1_Black_scores_max = GameTree board17 800 [ MoveSubtree board17_B1_Move (GameTree board17_B1_Board 800 [])
                                                            , MoveSubtree board17_B2_Move (GameTree board17_B2_Board (-300) [])
                                                            , MoveSubtree board17_B3_Move (GameTree board17_B3_Board (-800) [])
                                                            , MoveSubtree board17_B4_Move (GameTree board17_B4_Board (-1) [])]

-- GameTree starting with board16, of depth 1, where White goes first, with fake scores (minimized)
gameTree17_depth1_White_scores_min = GameTree board17 (-1) [ MoveSubtree board17_W1_Move (GameTree board17_W1_Board 50 [])
                                                         , MoveSubtree board17_W2_Move (GameTree board17_W2_Board (-1) [])
                                                         , MoveSubtree board17_W3_Move (GameTree board17_W3_Board (-1) [])
                                                         , MoveSubtree board17_W4_Move (GameTree board17_W4_Board (-1) [])
                                                         , MoveSubtree board17_W5_Move (GameTree board17_W5_Board 50 [])]

-- GameTree starting with board16, of depth 1, where Black goes first, with fake scores (minimized)
gameTree17_depth1_Black_scores_min = GameTree board17 (-800) [ MoveSubtree board17_B1_Move (GameTree board17_B1_Board 800 [])
                                                            , MoveSubtree board17_B2_Move (GameTree board17_B2_Board (-300) [])
                                                            , MoveSubtree board17_B3_Move (GameTree board17_B3_Board (-800) [])
                                                            , MoveSubtree board17_B4_Move (GameTree board17_B4_Board (-1) [])]
