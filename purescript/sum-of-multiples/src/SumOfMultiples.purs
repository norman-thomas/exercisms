module SumOfMultiples
  ( sumOfMultiples
  ) where

import Prelude

import Data.Array ((..), filter, any)
import Data.Foldable (sum)

sumOfMultiples :: Array Int -> Int -> Int
sumOfMultiples mults n = sum $ filter (isMultOf mults) (1 .. (n-1))

isMultOf :: Array Int -> Int -> Boolean
isMultOf mults n = any (\i -> n `mod` i == 0) mults
