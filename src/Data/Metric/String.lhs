Metric spaces defined over strings; i.e. edit distance.

> module Data.Metric.String (Hamming(..), Levenshtein(..), RestrictedDamerauLevenshtein(..)) where
>
> import Control.Applicative.Extras ((<$$>))
> import Data.Default (Default(def))
> import Data.Default.Instances.EditDistance ()
> import Data.Function (on)
> import Data.List.Extras (count)
> import Data.Metric.Class (Metric(..))
> import Text.EditDistance (levenshteinDistance, restrictedDamerauLevenshteinDistance)

  * Number of positions at which the strings differ.
  * Only a metric space for strings of fixed length.

> newtype Hamming = Hamming
>   { getHamming :: String
>   } deriving (Eq, Show)
> 
> instance Metric Hamming where
>   distance = count . filter id <$$> zipWith (/=) `on` getHamming

  * The number of deletion, insertion, and substitution operations needed for equality.

> newtype Levenshtein = Levenshtein
>   { getLevenshtein :: String
>   } deriving (Eq, Show)
>
> instance Metric Levenshtein where
>   distance = fromIntegral <$$> levenshteinDistance def `on` getLevenshtein

  * Levenshtein distance with the transposition operation.
  * Restricted: meaning no substring is edited more than once.

> newtype RestrictedDamerauLevenshtein = RestrictedDamerauLevenshtein
>   { getRestrictedDamerauLevenshtein :: String
>   } deriving (Eq, Show)
>
> instance Metric RestrictedDamerauLevenshtein where
>   distance = fromIntegral <$$> restrictedDamerauLevenshteinDistance def `on` getRestrictedDamerauLevenshtein
