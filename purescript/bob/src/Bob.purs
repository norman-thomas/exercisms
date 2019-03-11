module Bob
  ( hey
  ) where

import Prelude

import Data.Maybe (Maybe(..))
import Data.String (length, toUpper, trim, null)
import Data.String.CodeUnits (charAt)
import Data.String.Regex (test)
import Data.String.Regex.Flags (noFlags)
import Data.String.Regex.Unsafe (unsafeRegex)
import Type.Data.Boolean (kind Boolean)

hey :: String -> String
hey s | isSilence s  = "Fine. Be that way!"
      | isYell s     = "Whoa, chill out!"
      | isQuestion s = "Sure."
      | otherwise    = "Whatever."

isQuestion :: String -> Boolean
isQuestion = endsWith '?'

isYell :: String -> Boolean
isYell s = (s == toUpper s) && not (isOnlyNumbers s)

isSilence :: String -> Boolean
isSilence = trim >>> null

isOnlyNumbers :: String -> Boolean
isOnlyNumbers = checkRegex "^[0-9,.!?\\s]+$"

checkRegex :: String -> String -> Boolean
checkRegex r s = let rgx = unsafeRegex r noFlags
                 in test rgx s 

endsWith :: Char -> String -> Boolean
endsWith end s = (Just end) == charAt (length s - 1) s
