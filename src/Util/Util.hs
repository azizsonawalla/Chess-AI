module Util where

import Data.Char


-- Returns the first element of a 3-tuple
getFirst (a, b, c) = a

-- Converts a string to upper case
toUpperStr str = map toUpper str