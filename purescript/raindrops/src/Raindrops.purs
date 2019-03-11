module Raindrops
  ( raindrops
  ) where

import Prelude
import Data.Tuple (Tuple(..))
import Data.Foldable (foldl)

raindrops :: Int -> String
raindrops n = case result of
  "" -> show n
  _  -> result
  where
    result = foldl (<>) "" $ makeSound n <$> rules

divisibleBy :: Int -> Int -> Boolean
divisibleBy n m = (n `mod` m) == 0

rules :: Array (Tuple Int String)
rules = [ Tuple 3 "Pling"
        , Tuple 5 "Plang"
        , Tuple 7 "Plong"
        ]

makeSound :: Int -> Tuple Int String -> String
makeSound n (Tuple m s) | n `divisibleBy` m = s
                        | otherwise         = ""
