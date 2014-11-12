> module Data.Semimetric (
>   Semimetric(..)
> ) where
>
> import Data.Premetric (Premetric)

The `Semimetric` typeclass, as defined here, is intended to contain types that
are semimetric spaces. Instances can be defined in terms of `distance` or the
infix `<->`. 

> class Premetric a => Semimetric a where {}

In addition to properties of premetrics, these should satisfy symmetry, i.e:

  1. forall x. y. distance x y == distance y x
