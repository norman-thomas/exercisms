module Triangle
  ( Triangle(Equilateral, Isosceles, Scalene)
  , triangleKind
  ) where

import Prelude

import Data.Either (Either(..))

data Triangle = Equilateral | Isosceles | Scalene

derive instance triangleEq :: Eq Triangle

instance showTriangle :: Show Triangle where
  show Equilateral = "Equilateral"
  show Isosceles = "Isosceles"
  show Scalene = "Scalene"

triangleKind :: Int -> Int -> Int -> Either String Triangle
triangleKind a b c | isInvalid a b c        = Left "Invalid lengths"
triangleKind a b c | violatesEquality a b c = Left "Violates inequality"
triangleKind a b c | isEquilateral a b c    = Right Equilateral
triangleKind a b c | isIsosceles a b c      = Right Isosceles
triangleKind _ _ _                          = Right Scalene

isInvalid :: Int -> Int -> Int -> Boolean
isInvalid a b c | a <= 0 || b <= 0 || c <= 0 = true
isInvalid _ _ _                              = false

isEquilateral :: Int -> Int -> Int -> Boolean
isEquilateral a b c = a == b && b == c

isIsosceles :: Int -> Int -> Int -> Boolean
isIsosceles a b c = a == b || b == c || a == c

violatesEquality :: Int -> Int -> Int -> Boolean
violatesEquality a b c = a + b <= c || b + c <= a || a + c <= b
