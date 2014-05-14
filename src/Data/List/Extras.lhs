> module Data.List.Extras (count) where

`count` is `length` with a numeric result type:

> count :: Num b => [a] -> b
> count = fromIntegral . length
