module Advent.A03 where

import Prelude hiding (hGetContents)
import System.IO.Strict (hGetContents)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as H

split :: [a] -> ([a],[a])
split = foldr (\a ~(bb,aa) -> (a:aa,bb)) ([],[])

doFile :: FilePath -> IO (Int,Int)
doFile path = withFile path ReadMode \h ->
    hGetContents h <&> solve

type Pt = (Int,Int)
type St = HashMap Pt Int

move :: Pt -> Char -> Pt
move (!x,!y) = \case
    '>' -> (x+1,y)
    '<' -> (x-1,y)
    '^' -> (x,y+1)
    'v' -> (x,y-1)

path :: String -> [Pt]
path cc = pp where
    pp = (0,0) : zipWith move pp cc

update :: St -> Pt -> St
update h p = H.insertWith (+) p 1 h

solve :: String -> (Int,Int)
solve cc = (one,two)
  where
    deliver h = foldl' update h . path
    one = H.size $ deliver H.empty cc
    two = H.size $ deliver (deliver H.empty s) r
    (s,r) = split cc
