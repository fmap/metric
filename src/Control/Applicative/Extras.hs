module Control.Applicative.Extras ((<$$>), (<$$$>)) where

(<$$$>) :: (Functor f, Functor g, Functor h) => (a -> b) -> f(g(h a)) -> f(g(h b))
(<$$$>) = fmap . fmap . fmap
 
(<$$>) :: (Functor f, Functor g) => (a -> b) -> f (g a) -> f (g b)
(<$$>) = fmap . fmap

infixl 4 <$$>
infixl 8 <$$$>
