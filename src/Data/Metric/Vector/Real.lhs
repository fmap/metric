> {-# LANGUAGE GeneralizedNewtypeDeriving #-}

Metric spaces defined over real vectors.

> module Data.Metric.Vector.Real (
>   Euclidean(..),
>   Taxicab(..),
>   Chebyshev(..),
>   PostOffice(..)
> ) where
> 
> import Prelude hiding (zipWith, map, maximum, length, sum)
> import Data.Function (on)
> import Data.Packed.Matrix.Extras (fromVectors)
> import Data.Vector (Vector(..), zipWith, map, maximum, length, sum)
> import Data.Vector.Extras (zero)
> import Numeric.LinearAlgebra.Algorithms (rank)
> import Data.Metric.Class (Metric(..))
> import Control.Applicative.Extras ((<$$>), (<$$$>))

Real vectors can be viewed as a metric space in more than one way, as we can
define multiple valid distance functions. To avoid ambiguous type instances,
we define a newtype wrapper over `Vector Double` for each distance function,
and make that `newtype` an instance of `Metric`.
 
`Euclidean` wraps Euclidean distance, our usual conception of distance
between two points on a plane. The square root of the sum of the
differences between corresponding coordinates ;-).

> newtype Euclidean = Euclidean 
>   { getEuclidean :: Vector Double 
>   } deriving (Eq, Show)
> 
> instance Metric Euclidean where
>   distance = sqrt . sum . map (**2) <$$> zipWith (-) `on` getEuclidean

`Taxicab` describes the length of the path connecting the two vectors
along only vertical and horizontal lines (`_|`) without backtracing.
The name stems from the observation that it's the shortest distance you
could travel by taxi on a rectangular grid of streets (think Manhattan.)

> newtype Taxicab = Taxicab
>   { getTaxicab :: Vector Double
>   } deriving (Eq, Show)
> 
> instance Metric Taxicab where
>   distance = sum . map abs <$$> zipWith (-) `on` getTaxicab

`Chebyshev` wraps Chebyshev distance, in which the distance between two
vectors is defined to be the maximum of their differences along any
dimension.

> newtype Chebyshev = Chebyshev
>   { getChebyshev :: Vector Double
>   } deriving (Eq, Show)
> 
> instance Metric Chebyshev where
>   distance = maximum . map abs <$$> zipWith (-) `on` getChebyshev

Post Office distance; the Euclidean distance between two vectors is Euclidean
when they are co-linear with the origin, and otherwise the sum of their
magnitudes. It's named so as letters usually need to first travel to the post
office, with exception to when the letter passes the destination on the way
there.

Three points are considered to be colinear if the matrix of their coordinates
has a rank less than one. In constructing a vector here, we consider together
with the origin those points that are the subject of the metric.

> newtype PostOffice = PostOffice
>   { getPostOffice :: Vector Double
>   } deriving (Eq, Show)
> 
> instance Metric PostOffice where
>   PostOffice v0 <-> PostOffice v1 
>     | colinear v0 v1 (zero $ length v0) = Euclidean v0 <-> Euclidean v1
>     | otherwise = v0 |+| v1
>
> colinear :: Vector Double -> Vector Double -> Vector Double -> Bool
> colinear = (<1) . rank <$$$> fromVectors
>
> (|+|) :: Vector Double -> Vector Double -> Double
> (|+|) = (+) `on` mag
>
> mag :: Vector Double -> Double
> mag = sqrt . sum . map (**2)
