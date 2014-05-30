module Data.Metric (
  Metric(..),
  Discrete(..),
  Hamming(..),
  Levenshtein(..),
  RestrictedDamerauLevenshtein(..),
  Euclidean(..),
  Taxicab(..),
  Chebyshev(..),
  PostOffice(..)
) where

import Data.Metric.Class (Metric(..))
import Data.Metric.Set (Discrete(..))
import Data.Metric.String (Hamming(..), Levenshtein(..), RestrictedDamerauLevenshtein(..)) 
import Data.Metric.Vector.Real (Euclidean(..), Taxicab(..), Chebyshev(..), PostOffice(..))
