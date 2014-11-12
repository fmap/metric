> module Data.Premetric (
>   Premetric(..)
> ) where

The `Premetric` typeclass, as defined here, is intended to contain types that
are premetric spaces. Instances can be defined in terms of `distance` or the
infix `<->`. 

> class Premetric a where
>   (<->) :: a -> a -> Double
>   (<->) = distance 
>   distance :: a -> a -> Double
>   distance = (<->)

These should satisfy non-negativity and the identity of indiscernables, i.e:
  
  1. forall x. y. distance x y >= 0
  2. forall x. y. distance y x == 0 <=> x == y
