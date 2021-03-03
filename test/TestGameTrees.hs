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

-- GameTree starting with board16, of depth 1, where White goes first, with fake scores
gameTree16_depth1_White_scores = GameTree board16 0 [ MoveSubtree board16_W1_Move (GameTree board16_W1_Board 50 [])
                                                    , MoveSubtree board16_W2_Move (GameTree board16_W2_Board (-1) [])]

-- GameTree starting with board16, of depth 1, where Black goes first, with fake scores
gameTree16_depth1_Black_scores = GameTree board16 0 [ MoveSubtree board16_B1_Move (GameTree board16_B1_Board 1 [])
                                                    , MoveSubtree board16_B2_Move (GameTree board16_B2_Board (-32) [])
                                                    , MoveSubtree board16_B3_Move (GameTree board16_B3_Board 50 [])
                                                    , MoveSubtree board16_B4_Move (GameTree board16_B4_Board (-81) [])
                                                    , MoveSubtree board16_B5_Move (GameTree board16_B5_Board 200 [])
                                                    , MoveSubtree board16_B6_Move (GameTree board16_B6_Board 500 [])
                                                    , MoveSubtree board16_B7_Move (GameTree board16_B7_Board (-800) [])
                                                    , MoveSubtree board16_B8_Move (GameTree board16_B8_Board 501 [])]