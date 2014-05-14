> module Data.Default.Instances.EditDistance () where
>
> import Data.Default (Default(..))
> import Text.EditDistance (Costs(..), EditCosts(..))

For sake of simplicity, we associate each operation with a fixed edit
cost:

> instance Default (Costs a) where
>   def = ConstantCost 1
>
> instance Default EditCosts where
>   def = EditCosts def def def def
