module Util where

import Data.Char
import Data.List


-- Returns the first element of a 3-tuple
-- TODO: test
getFirst (a, b, c) = a

-- Converts a string to upper case
-- TODO: test
toUpperStr str = map toUpper str

-- TODO: test
split deliminator str = foldl (\ list char -> if char==deliminator then (list++[""]) else (init list)++[(last list)++[char]]) [""] str

subset :: (Eq a, Ord a) => [a] -> [a] -> Bool
subset list1 list2 = (list1 /= []) && ((sort (intersect list1 list2)) == (sort list1))