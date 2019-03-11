module Acronym
  ( abbreviate
  ) where

import Prelude (($), map, flip)
import Data.Array.NonEmpty (NonEmptyArray, toArray)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String (joinWith, toUpper)
import Data.String.Regex (match)
import Data.String.Regex.Flags (global)
import Data.String.Regex.Unsafe (unsafeRegex)

abbreviate :: String -> String
abbreviate s = toUpper $ joinWith "" $ importantLetters s

importantLetters :: String -> Array String
importantLetters s = toStringArray $ flip match s $ unsafeRegex pattern global
  where pattern = "\\b\\w|[A-Z]"

toStringArray :: Maybe (NonEmptyArray (Maybe String)) -> Array String
toStringArray Nothing = []
toStringArray (Just a) = map (fromMaybe "") $ toArray a
