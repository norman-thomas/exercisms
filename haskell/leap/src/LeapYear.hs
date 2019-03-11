module LeapYear (isLeapYear) where

isLeapYear :: Integer -> Bool
isLeapYear year | year `mod` 400 == 0 = True
                | year `mod` 100 == 0 = False
                | otherwise           = year `mod` 4 == 0
