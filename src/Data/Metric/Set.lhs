> {-# LANGUAGE GADTs #-}

Metric spaces defined over arbitrary sets.

> module Data.Metric.Set (
>   Discrete(..)
> ) where
>
> import Control.Applicative.Extras ((<$$>))
> import Data.Metric.Class (Metric(..))

`Discrete` wraps the discrete metric. If to elements are equal, the
distance is 0, otherwise it is 1. This can be applied between any
two `Eq` instances; i.e. any pair of non-empty sets.

> data Discrete a where 
>   Element :: Eq a => a -> Discrete a
>
> instance Metric (Discrete a) where
>   Element a <-> Element b
>     | a == b    = 0
>     | otherwise = 1
>
> instance Eq (Discrete a) where
>   (==) = (0==) <$$> distance
>
> instance Show a => Show (Discrete a) where
>   show (Element a) = "Discrete " ++ show a
