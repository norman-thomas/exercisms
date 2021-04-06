module Anagram (anagramsFor) where

import Data.Char (toLower)
import Data.List (sort)


anagramsFor :: String -> [String] -> [String]
anagramsFor xs xss = filter (isAnagramOf xs) xss

isAnagramOf :: String -> String -> Bool
isAnagramOf a b = aLower /= bLower && sort aLower == sort bLower
                    where
                        aLower = map toLower a
                        bLower = map toLower b
