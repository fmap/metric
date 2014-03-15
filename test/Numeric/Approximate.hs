module Numeric.Approximate (Approximate(..)) where

class Approximate a where
  (=~)  :: a -> a -> Bool
  (<=~) :: a -> a -> Bool
  (>=~) :: a -> a -> Bool

instance Approximate Double where
  a =~ b  = abs (b-a) < 0.001
  a <=~ b = a=~b || b-a > 0
  a >=~ b = a=~b || b-a < 0
