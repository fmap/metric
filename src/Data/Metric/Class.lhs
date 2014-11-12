> module Data.Metric.Class (
>   Metric(..)
> ) where
>
> import Data.Semimetric (Semimetric(..))

The `Metric` typeclass, as defined here, is intended to contain types that
are metric spaces. Instances can be defined in terms of `distance` or the 
infix `<->`:

> class Semimetric a => Metric a where
>   (<->) :: a -> a -> Double
>   (<->) = distance 
>   distance :: a -> a -> Double
>   distance = (<->)

In addition to properties of semimetrics, these should satisfy triangle
inequality, i.e:
  
  1. forall x. y. z. distance x z <= distance x y + distance y z
