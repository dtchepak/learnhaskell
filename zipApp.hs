-- http://conal.net/blog/posts/more-beautiful-fold-zipping
import Control.Applicative
import Prelude hiding (zip)

class Zip f where
    zip :: f a -> f b -> f (a,b)

apply :: (Functor f, Zip f) => f (a -> b) -> f a -> f b
apply f a =
    fmap (\(f',a') -> f' a') (zip f a)

liftZ3 :: (Functor f, Zip f) => (a -> b -> c) -> f a -> f b -> f c
liftZ3 f a b = f `fmap` a `apply` b

zipA :: Applicative f => f a -> f b -> f (a,b)
zipA a b = (,) <$> a <*> b

