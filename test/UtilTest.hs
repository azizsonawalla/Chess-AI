{-# OPTIONS_GHC -F -pgmF htfpp #-}

import Test.Framework
import Util

test_subset = 
    do
        assertEqual False (subset [] [1, 2, 3, 4])
        assertEqual False (subset [] [('A', 1), ('B', 2)])
        assertEqual True (subset [1, 2] [56, 34, 3, 2, 1]) 
        assertEqual False (subset [5, 7] [1, 2, 3, 4])

main = htfMain htf_thisModulesTests