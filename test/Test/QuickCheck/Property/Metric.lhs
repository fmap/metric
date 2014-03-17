All instances of Metric are expected to have the properties of metric
spaces. Precisely; given a tuple (M, d), where M is a set and d a
function (M -> M), forall a,b,c in M:

> module Test.QuickCheck.Property.Metric (prop_Metric) where
> 
> import Data.Metric (Metric(..))
> import Test.QuickCheck (Property, conjoin, label)
> import Numeric.Approximate (Approximate(..)) -- Floating point comparison is unreliable. 

  * Non-Negativity: d(a,b) >= 0

> prop_MetricNegative :: Metric a => a -> a -> Bool
> prop_MetricNegative a b = (a <-> b) >=~ 0

  * Identity of Indiscernables: d(x,y) = 0 <=> x = y

> prop_MetricIndiscernable :: (Eq a, Metric a) => a -> a -> Bool
> prop_MetricIndiscernable a b = ((a <-> b) =~ 0) == (a == b)

  * Symmetry: d(x,y) = d(y,x)

> prop_MetricSymmetry :: Metric a => a -> a -> Bool
> prop_MetricSymmetry x y = (x <-> y) =~ (y <-> x)

  * Triangle Inequality: d(x,z) <= d(x,y) + d(y,z)
  
> prop_MetricTriangle :: Metric a => a -> a -> a -> Bool
> prop_MetricTriangle x y z = (x <-> z) <=~ ((x <-> y) + (y <-> z))

All together now:

> prop_Metric :: (Eq a, Metric a) => a -> a -> a -> Property
> prop_Metric x y z = conjoin $
>   [ label "Non-Negativity"             $ prop_MetricNegative x y
>   , label "Identity of Indiscernables" $ prop_MetricIndiscernable x y
>   , label "Symmetry"                   $ prop_MetricSymmetry x y
>   , label "Triangle Inequality"        $ prop_MetricTriangle x y z
>   ]
