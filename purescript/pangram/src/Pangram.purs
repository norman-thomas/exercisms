module Pangram
  ( isPangram
  ) where

import Prelude

import Data.Array (filter)
import Data.Char.Unicode (isAlpha, isAscii)
import Data.Set (fromFoldable, size)
import Data.String (toLower)
import Data.String.CodeUnits (toCharArray)


isPangram :: String -> Boolean
isPangram s = 26 == (size $ fromFoldable $ filter (\c -> isAlpha c && isAscii c) $ toCharArray $ toLower s)
