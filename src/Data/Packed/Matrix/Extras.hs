module Data.Packed.Matrix.Extras (fromVectors) where

import Data.Function.Extras (on3)
import Data.Vector (Vector(..), toList)
import Data.Packed.Matrix (Matrix(..), fromLists, trans)

fromVectors :: Vector Double -> Vector Double -> Vector Double -> Matrix Double
fromVectors = fromColumns `on3` toList
  where fromColumns xs ys zs = trans $ fromLists [xs, ys, zs]
