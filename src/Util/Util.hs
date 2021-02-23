module Util where

import Data.Char


-- Returns the first element of a 3-tuple
getFirst (a, b, c) = a

-- Converts a string to upper case
toUpperStr str = map toUpper str

split deliminator str = foldl (\ list char -> if char==deliminator then (list++[""]) else (init list)++[(last list)++[char]]) [""] str