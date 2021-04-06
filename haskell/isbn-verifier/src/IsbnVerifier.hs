module IsbnVerifier (isbn) where

import Data.Char (digitToInt)
import Control.Arrow ((>>>))

isbn :: String -> Bool
isbn s = verifyLength i && verifyDigits i && verifyChecksum i
            where
                i = stringToIntArray s


verifyLength :: [a] -> Bool
verifyLength = length >>> (== 10)


verifyDigits :: [Int] -> Bool
verifyDigits s = allButLastDigit && lastDigit
                where
                    allButLastDigit = all (< 10) $ init s
                    lastDigit = elem (last s) [0..10]


verifyChecksum :: [Int] -> Bool
verifyChecksum =  checksum >>> (== 0)


stringToIntArray :: String -> [Int]
stringToIntArray = concat . map charToInt


charToInt :: Char -> [Int]
charToInt c = case c of
                'X' -> [10]
                c | elem c ['0'..'9'] -> [digitToInt c]
                  | otherwise -> []


checksum :: [Int] -> Int
checksum is = (sum $ map (\(a, b) -> a * b) (zip digits is)) `mod` 11
                where
                    digits = reverse [1..length is]
