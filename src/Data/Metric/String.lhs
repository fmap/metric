Metric spaces defined over strings; i.e. edit distance.

> module Data.Metric.String (
>   Hamming(..),
>   Levenshtein(..),
>   RestrictedDamerauLevenshtein(..)
> ) where
>
> import Data.Default (Default(def))
> import Data.Default.Instances.EditDistance ()
> import Data.Function (on)
> import Data.Functor.Infix ((<$$>))
> import Data.List.Extras (count)
> import Data.Metric.Class (Metric(..))
> import Data.Metric.Set (Discrete(..))
> import Text.EditDistance (levenshteinDistance, restrictedDamerauLevenshteinDistance)

`Hamming` wraps Hamming distance: the number of positions between two
*fixed length* strings at which the corresponding symbols are different.
Equivalently, this can be seen as the minimum number of substitutions
required to change one of the strings into the other.

> newtype Hamming = Hamming
>   { getHamming :: String
>   } deriving (Eq, Show)
> 
> instance Metric Hamming where
>   distance = count <$$> zipWith (distance `on` Discrete) `on` getHamming

`Levenshtein` wraps Levenshtein distance: the minimum number of
single-character operations-- insertions, deletions or substitutions--
required to make two strings equal. Here, we in effect re-export the
heavily optimised implementation provided by Max Bolingbroke's excellent
`edit-distance` package.

> newtype Levenshtein = Levenshtein
>   { getLevenshtein :: String
>   } deriving (Eq, Show)
>
> instance Metric Levenshtein where
>   distance = fromIntegral <$$> levenshteinDistance def `on` getLevenshtein

The Restricted-Damerau Levenshtein distance extends Levenshtein distance
(above), with an additional operation: the transposition of two
adjacent characters. It is 'restricted' in that the algorithm computes
the minimum number of edit operations **under the condition that no
substring is edited more than once.** Here, again, we're wrapping an
implementation from `edit-distance`.

> newtype RestrictedDamerauLevenshtein = RestrictedDamerauLevenshtein
>   { getRestrictedDamerauLevenshtein :: String
>   } deriving (Eq, Show)
>
> instance Metric RestrictedDamerauLevenshtein where
>   distance = fromIntegral <$$> restrictedDamerauLevenshteinDistance def `on` getRestrictedDamerauLevenshtein
