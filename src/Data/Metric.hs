module Data.Metric (
  Metric(..),
  Discrete(..),
  Hamming(..),
  Levenshtein(..),
  RestrictedDamerauLevenshtein(..),
  Euclidean(..),
  Taxicab(..),
  Cosine(..),
  Chebyshev(..),
  PostOffice(..)
) where

import Data.Metric.Class (Metric(..))
import Data.Metric.Set (Discrete(..))
import Data.Metric.String (Hamming(..), Levenshtein(..), RestrictedDamerauLevenshtein(..)) 
import Data.Metric.Vector.Real (Euclidean(..), Taxicab(..), Cosine(..), Chebyshev(..), PostOffice(..))
