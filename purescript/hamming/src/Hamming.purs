module Hamming
  ( distance
  ) where

import Prelude

import Data.Array (zipWith)
import Data.Foldable (sum)
import Data.Maybe (Maybe(..))
import Data.String (length)
import Data.String.CodeUnits (toCharArray)
import Type.Data.Boolean (kind Boolean)

distance :: String -> String -> Maybe Int
distance a b | length a == length b = Just $ sum $ toInt <$> diff (toCharArray a) (toCharArray b)
             | otherwise            = Nothing

diff :: forall a. Eq a => Array a -> Array a -> Array Boolean
diff a b = zipWith (/=) a b

toInt :: Boolean -> Int
toInt true = 1
toInt _    = 0
