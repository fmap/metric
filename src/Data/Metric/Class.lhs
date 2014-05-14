> module Data.Metric.Class (
>   Metric(..)
> ) where

The `Metric` typeclass, as defined here, is intended to contain types that
are metric spaces. Instances can be defined in terms of `distance` or the 
infix `<->`:

> class Metric a where
>   (<->) :: a -> a -> Double
>   (<->) = distance 
>   distance :: a -> a -> Double
>   distance = (<->)
