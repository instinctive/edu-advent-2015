module Advent.A02 where

import Prelude hiding (hGetContents)

import Data.List.Split (splitOn)
import System.IO.Strict (hGetContents)

doFile :: FilePath -> IO (Int,Int)
doFile path = withFile path ReadMode \h ->
    hGetContents h <&> foldl' f (0,0) . map solve . lines
  where
    f (!u,!v) (s,t) = (u+s,v+t)

solve :: String -> (Int,Int)
solve s = (one,two)
  where
    abc@[a,b,c] = map read $ splitOn "x" s
    mm = zipWith (*) abc [b,c,a]
    pp = zipWith (+) abc [b,c,a]
    one = minimum mm + 2 * sum mm
    two = 2 * minimum pp + product abc
