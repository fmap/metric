Miscellaneous distance functions, specialised to real vectors.

> module Data.Metric (
>   Metric(..),
>   Discrete(..),
>   Euclidean(..),
>   Taxicab(..),
>   Cosine(..),
>   Chebyshev(..),
>   PostOffice(..)
> ) where
> 
> import Prelude hiding (zipWith, map, foldr1, maximum)
> import Data.Function (on)
> import Data.Vector (Vector(..), zipWith, map, foldr1, maximum)

The `Metric` typeclass, as defined here, is intended to contain types that
are metric spaces. Instances can be defined in terms of `distance` or the 
infix `<->`:

> class Metric a where
>   (<->) :: a -> a -> Double
>   (<->) = distance 
>   distance :: a -> a -> Double
>   distance = (<->)
 
Real vectors can be viewed as a metric space in more than one way, as we can
define multiple valid distance functions. To avoid ambiguous type instances,
we define a newtype wrapper over `Vector Double` for each distance function,
and make that `newtype` an instance of `Metric`.

`Discrete` wraps the discrete metric. If to elements are equal, the
distance is 0, otherwise it is 1. This *could* be applied between any
two `Eq` instances, but here it is specialised for want of a consistent
interface.

> newtype Discrete = Discrete
>   { getDiscrete :: Vector Double
>   }
> 
> instance Metric Discrete where
>   Discrete v0 <-> Discrete v1 
>     | v0 == v1  = 0 
>     | otherwise = 1
 
`Euclidean` wraps Euclidean distance, our usual conception of distance
between two points on a plane. The square root of the sum of the
differences between corresponding coordinates ;-).

> newtype Euclidean = Euclidean 
>   { getEuclidean :: Vector Double 
>   }
> 
> instance Metric Euclidean where
>   Euclidean v0 <-> Euclidean v1 = sqrt . foldr1 (+) . map (**2) $ zipWith (-) v0 v1

`Taxicab` describes the length of the path connecting the two vectors
along only vertical and horizontal lines (`_|`) without backtracing.
The name stems from the observation that it's the shortest distance you
could travel by taxi on a rectangular grid of streets (think Manhattan.)

> newtype Taxicab = Taxicab
>   { getTaxicab :: Vector Double
>   }
> 
> instance Metric Taxicab where
>   Taxicab v0 <-> Taxicab v1 = foldr1 (+) . map abs $ zipWith (-) v0 v1
 
`Cosine` wraps Cosine similarity, measuring the cosine of the angle
between two vectors using the Euclidean dot product formula. Unlike the
other distance functions, this described orientation, rather than
magnitude; this is useful when comparing tf-idf weights, as we're
implicitly normalising for document length.

> newtype Cosine = Cosine
>   { getCosine :: Vector Double
>   }
> 
> instance Metric Cosine where
>   Cosine v0 <-> Cosine v1 = (v0 `dot` v1) / (v0 |*| v1)
>
> mag :: Vector Double -> Double
> mag = sqrt . foldr1 (+) . map (**2)
>
> dot :: Vector Double -> Vector Double -> Double
> dot = foldr1 (+) .:. zipWith (*)
>
> (|*|) :: Vector Double -> Vector Double -> Double
> (|*|) = (*) `on` mag
> 
> (.:.) :: (Functor f, Functor g) => (a -> b) -> f (g a) -> f (g b)
> (.:.) = fmap . fmap

`Chebyshev` wraps Chebyshev distance, in which the distance between two
vectors is defined to be the maximum of their differences along any
dimension.

> newtype Chebyshev = Chebyshev
>   { getChebyshev :: Vector Double
>   }
> 
> instance Metric Chebyshev where
>   Chebyshev v0 <-> Chebyshev v1 = maximum . map abs $ zipWith (-) v0 v1

Post Office distance; zero if the vectors are equal, otherwise the sum
of their magnitudes. It's named so as letters usually need to first
travel to the post office, irrespective of their final destination,
provided the source and destination are distinct.

> newtype PostOffice = PostOffice
>   { getPostOffice :: Vector Double
>   }
> 
> instance Metric PostOffice where
>   PostOffice v0 <-> PostOffice v1 
>     | v0 == v1  = 0
>     | otherwise = v0 |+| v1
>
> (|+|) :: Vector Double -> Vector Double -> Double
> (|+|) = (+) `on` mag
