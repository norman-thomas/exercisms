module PrimeFactors (primeFactors) where

primeFactors :: Integer -> [Integer]
primeFactors n = factorize n 2

factorize :: Integer -> Integer -> [Integer]
factorize n m | m > n = []
factorize n m | n `mod` m == 0 = [m] ++ factorize (n `div` m) m
factorize n m = factorize n (step m)

step :: Integer -> Integer
step 2 = 3
step n = n + 2
