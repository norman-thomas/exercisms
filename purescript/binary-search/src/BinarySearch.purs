module BinarySearch
  ( find
  ) where

import Prelude
import Data.Array (length, unsafeIndex)
import Data.Maybe (Maybe(..))
import Partial.Unsafe (unsafePartial)

find :: forall a. Ord a => a -> Array a -> Maybe Int
find n l = find' 0 (length l - 1) n l

find' :: forall a. Ord a => Int -> Int -> a -> Array a -> Maybe Int
find' left right _ _ | left > right = Nothing
find' left right n l =
  if pivot == n then
    Just middle
  else if pivot > n then
    find' left (middle - 1) n l
  else
    find' (middle + 1) right n l
  where
    middle = (left + right) `div` 2
    pivot = unsafePartial $ unsafeIndex l middle
