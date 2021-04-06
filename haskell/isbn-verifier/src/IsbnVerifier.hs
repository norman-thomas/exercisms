module IsbnVerifier (isbn) where

import Data.Char (digitToInt)

isbn :: String -> Bool
isbn s = verifyLength i && verifyValidCharacters i && verifyChecksum i
            where i = removeDashes s


verifyLength :: String -> Bool
verifyLength s = length s == 10


verifyValidCharacters :: String -> Bool
verifyValidCharacters s = all (flip elem (['0'..'9'])) allButLastChar && elem lastChar (['0'..'9'] ++ ['X'])
                            where
                                allButLastChar = reverse $ tail $ reverse s
                                lastChar = last s


verifyChecksum :: String -> Bool
verifyChecksum s = (checksum $ toIntArray s) `mod` 11 == 0


removeDashes :: String -> String
removeDashes = filter (/= '-')


toIntArray :: String -> [Int]
toIntArray s = map toInt s
                where
                    toInt c = case c of
                                'X' -> 10
                                c -> digitToInt c

checksum :: [Int] -> Int
checksum is = sum $ map (\(a, b) -> a * b) (zip digits is)
                where
                    digits = reverse [1..length is]
