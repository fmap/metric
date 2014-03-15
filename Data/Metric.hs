module Data.Metric where

import Prelude hiding (zipWith, map, foldr1, minimum)
import Data.Vector (Vector(..), zipWith, map, foldr1, minimum)

class Metric a where
  (<->) :: a -> a -> Double
  (<->) = distance 
  distance :: a -> a -> Double
  distance = (<->)

newtype Discrete = Discrete
  { getDiscrete :: Vector Double
  }

instance Metric Discrete where
  Discrete v0 <-> Discrete v1 = if v0 == v1 then 0 else 1

newtype Euclidean = Euclidean 
  { getEuclidean :: Vector Double 
  }

instance Metric Euclidean where
  Euclidean v0 <-> Euclidean v1 = foldr1 (+) . map (**2) $ zipWith (-) v0 v1

newtype Taxicab = Taxicab
  { getTaxicab :: Vector Double
  }

instance Metric Taxicab where
  Taxicab v0 <-> Taxicab v1 = foldr1 (+) . map abs $ zipWith (-) v0 v1

newtype Cosine = Cosine
  { getCosine :: Vector Double
  }

dot :: Vector Double -> Vector Double -> Double
dot = foldr1 (+) <$$> zipWith (*)

(<$$>) :: (Functor f, Functor g) => (a -> b) -> f (g a) -> f (g b)
(<$$>) = fmap . fmap

norm :: Vector Double -> Double
norm = sqrt . foldr1 (+) . map (**2)

instance Metric Cosine where
  Cosine v0 <-> Cosine v1 = (v0 `dot` v1) / (norm v0 * norm v1)

newtype Chebyshev = Chebyshev
  { getChebyshev :: Vector Double
  }

instance Metric Chebyshev where
  Chebyshev v0 <-> Chebyshev v1 = minimum . map abs $ zipWith (-) v0 v1


