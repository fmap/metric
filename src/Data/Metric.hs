module Data.Metric (
  Metric(..),
  Discrete(..),
  Euclidean(..),
  Taxicab(..),
  Cosine(..),
  Chebyshev(..),
  PostOffice(..)
) where

import Data.Metric.Class (Metric(..))
import Data.Metric.Vector.Real (Discrete(..), Euclidean(..), Taxicab(..), Cosine(..), Chebyshev(..), PostOffice(..))
import Data.Metric.String () 
