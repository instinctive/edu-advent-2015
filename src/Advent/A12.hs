-- https://adventofcode.com/2015/day/12
-- https://hao.codes/haskell-advent.html

{-# LANGUAGE OverloadedStrings #-}

module Advent.A12 where

import Control.Lens (sumOf, cosmosOf, plate, filtered)
import Data.Aeson (Value(..), decodeFileStrict')
import Data.Aeson.Lens (_Number)
import Data.Scientific (Scientific)

solve :: FilePath -> IO (Maybe (Scientific,Scientific))
solve path = decodeFileStrict' path <&> fmap mk
  where
    mk v = (one v, two v)
    one = tally plate
    two = tally (plate . filtered nonred)
    tally f = sumOf (cosmosOf f . _Number)
    nonred (Object h) = notElem (String "red") h
    nonred _ = True
