{-# OPTIONS_GHC -F -pgmF htfpp #-}

import Test.Framework
import ChessBoard

-- TODO: add tests

test_sample = 
    do assertEqual 1 2

main = htfMain htf_thisModulesTests