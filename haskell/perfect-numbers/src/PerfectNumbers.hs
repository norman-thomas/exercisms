module PerfectNumbers (classify, Classification(..)) where

data Classification = Deficient | Perfect | Abundant deriving (Eq, Show)

classify :: Int -> Maybe Classification
classify n | n < 1             = Nothing
           | aliquotSum == n = Just Perfect
           | aliquotSum > n  = Just Abundant
           | otherwise         = Just Deficient
    where
        aliquotSum = sum $ factors n

factors :: Int -> [Int]
factors n = [f | f <- [1..n `div` 2], n `mod` f == 0]
