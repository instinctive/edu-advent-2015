module Advent.A01 where

solve :: String -> (Int,Int)
solve s = (one,two) where
    one = go 0 s
    two = base 0 $ zip [0..] (s <> "$") -- for last place
    go !n [] = n
    go !n (c:s) = go (dir c n) s
    dir '(' = succ
    dir ')' = pred
    base !n ((i,c):more)
        | n < 0 = i
        | otherwise = base (dir c n) more
