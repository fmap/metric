Metric spaces defined over strings; i.e. edit distance.

> module Data.Metric.String (
>   Hamming(..),
>   Levenshtein(..),
>   RestrictedDamerauLevenshtein(..)
> ) where
>
> import Data.Default (Default(..))
> import Data.Function (on)
> import Data.Functor.Infix ((<$$>))
> import Data.Metric.Class (Metric)
> import Data.Metric.Set (Discrete(..))
> import Data.Premetric (Premetric(..))
> import Data.Semimetric (Semimetric)
> import Text.EditDistance (levenshteinDistance, restrictedDamerauLevenshteinDistance, Costs(..), EditCosts(..))

`Hamming` wraps Hamming distance: the number of positions between two
*fixed length* strings at which the corresponding symbols are different.
Equivalently, this can be seen as the minimum number of substitutions
required to change one of the strings into the other.

> newtype Hamming = Hamming
>   { getHamming :: String
>   } deriving (Eq, Show)
>
> instance Premetric Hamming where
>   distance = count <$$> zipWith (distance `on` Discrete) `on` getHamming
>
> instance Semimetric Hamming
> instance Metric Hamming
>
> count :: Num b => [a] -> b
> count = fromIntegral . length

`Levenshtein` wraps Levenshtein distance: the minimum number of
single-character operations-- insertions, deletions or substitutions--
required to make two strings equal. Here, we in effect re-export the
heavily optimised implementation provided by Max Bolingbroke's excellent
`edit-distance` package.

> newtype Levenshtein = Levenshtein
>   { getLevenshtein :: String
>   } deriving (Eq, Show)
>
> instance Premetric Levenshtein where
>   distance = fromIntegral <$$> levenshteinDistance def `on` getLevenshtein
>
> instance Semimetric Levenshtein
> instance Metric Levenshtein

For sake of simplicity, we have associated each operation with a fixed edit
cost:

> instance Default (Costs a) where
>   def = ConstantCost 1
>
> instance Default EditCosts where
>   def = EditCosts def def def def

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
> instance Premetric RestrictedDamerauLevenshtein where
>   distance = fromIntegral <$$> restrictedDamerauLevenshteinDistance def `on` getRestrictedDamerauLevenshtein
>
> instance Semimetric RestrictedDamerauLevenshtein
> instance Metric RestrictedDamerauLevenshtein
>
