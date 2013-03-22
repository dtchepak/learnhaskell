data Free t a =
    Done a
    | More (t (Free t a))

instance Functor f => Functor (Free f) where
    fmap f (Done a) = Done (f a)
    fmap f (More fa) = More . (fmap . fmap $ f) $ fa

instance Functor f => Monad (Free f) where
    return = Done
    (Done a) >>= fn = fn a
    (More fa) >>= fn = More $ fmap (>>= fn) fa

data CoFree t a = CoFree a (t (CoFree t a)) -- (a, t (CoFree t a))

class Comonad c where
    extract :: c a -> a
    duplicate :: c a -> c (c a)
    extend :: (c a -> b) -> c a -> c b
    (=>>) :: c a -> (c a -> b) -> c b
    (=>>) = flip extend

instance Functor f => Functor (CoFree f) where
    fmap f (CoFree a fca) = CoFree (f a) ((fmap.fmap) f fca)

instance Functor f => Comonad (CoFree f) where
    extract (CoFree a _) = a
    duplicate ca@(CoFree a fca) = CoFree ca (fmap duplicate fca)
    extend f ca = fmap f (duplicate ca)

