module Leap where

import Prelude

import Data.Int (rem)

isLeapYear :: Int -> Boolean
isLeapYear y | y `rem` 400 == 0 = true
             | y `rem` 100 == 0 = false
             | y `rem` 4 > 0    = false
             | otherwise        = true
