> module Numeric.Approximate (Approximate(..)) where
 
Given the necessary imprecision of floating point values, we use a compensatory 
notion of ordering when testing structure properties.

> class Approximate a where
>   (=~)  :: a -> a -> Bool
>   (<=~) :: a -> a -> Bool
>   (>=~) :: a -> a -> Bool

Two doubles are approximately equal if the difference between them is less than
1e-3. The bounds for (>=, =<) are modified based on this definition of equality.

> instance Approximate Double where
>   a =~ b  = abs (b-a) < 0.001
>   a <=~ b = a=~b || b-a > 0
>   a >=~ b = a=~b || b-a < 0
