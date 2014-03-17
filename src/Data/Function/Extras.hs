module Data.Function.Extras (on3) where

on3 :: (b -> b -> b -> c) -> (a -> b) -> a -> a -> a -> c
g `on3` f = \x y z -> g (f x) (f y) (f z)
