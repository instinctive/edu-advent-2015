-- https://adventofcode.com/2015/day/12
-- https://hao.codes/haskell-advent.html

{-# LANGUAGE OverloadedStrings #-}

module A12 where

import Control.Lens (sumOf, cosmosOf, plate, filtered)
import Data.Aeson (Value(..), decodeFileStrict')
import Data.Aeson.Lens (_Number)

main :: IO ()
main = getArgs >>= traverse_ go where
    go path = decodeFileStrict' path >>= print . (path,) . fmap mk
    mk v = (one v, two v)
    one = tally plate
    two = tally (plate . filtered nonred)
    tally f = sumOf (cosmosOf f . _Number)
    nonred (Object h) = notElem (String "red") h
    nonred _ = True
