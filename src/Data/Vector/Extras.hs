module Data.Vector.Extras (zero) where

import Prelude hiding(replicate)
import Data.Vector (Vector(..), replicate)

zero :: Num a => Int -> Vector a
zero = flip replicate 0
