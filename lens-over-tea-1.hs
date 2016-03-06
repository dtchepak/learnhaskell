{-# LANGUAGE
RankNTypes,
TupleSections,
DeriveFunctor
  #-}

-- from: https://artyom.me/lens-over-tea-1

import Control.Applicative
import Control.Monad (join)

type Lens s t a b = forall f. Functor f => (a -> f b) -> s -> f t
type Lens' s a = Lens s s a a

-- _1 :: Functor f => (a -> f b) -> (a, x) -> f (b, x)
_1 :: Lens (a, x) (b, x) a b
_1 f (a,x) = (, x) <$> f a

-- _2 :: Functor f => (a -> f b) -> (x, a) -> f (x, b)
_2 :: Lens (x, a) (x, b) a b
_2 f (x,a) = (x,) <$> f a

-- Make a lens out of a getter and a setter.
lens :: (s -> a) -> (s -> b -> t) -> Lens s t a b
lens get set f s =
    let a = get s
        fb = f a
    in set s <$> fb

-- Combine 2 lenses to make a lens which works on Either. (It's a good idea
-- to try to use bimap for this, but it won't work, and you have to use
-- explicit case-matching. Still a good idea, tho.)
choosing :: Lens s1 t1 a b -> Lens s2 t2 a b
         -> Lens (Either s1 s2) (Either t1 t2) a b
-- choosing::
--  ((a -> f b) -> s1 -> f t1)
--  -> ((a -> f b) -> s2 -> f t2)
--  -> ((a -> f b) -> (Either s1 s2) -> f (Either t1 t2)
choosing l1 l2 f =
    either
        (fmap Left . l1 f)
        (fmap Right . l2 f)


data Identity a = Identity { runIdentity :: a }
    deriving (Eq, Show, Functor)

data Pair f g a = Pair (f a) (g a)
    deriving (Eq, Show, Functor)

-- Modify the target of a lens and return the result. (Bonus points if you
-- do it without lambdas and defining new functions. There's also a hint
-- before the end of the section, so don't scroll if you don't want it.)
(<%~) :: Lens s t a b -> (a -> b) -> s -> (b, t)
-- ((a -> f b) -> s -> f t)
-- -> (a -> b)
-- -> s
-- -> (b, t)
(<%~) l f =
        l (join (,) . f)
-- Attempt 1:
--    l (\a -> (f a, f a))
-- Attempt 0:
--    let (Pair x y) = l (\a -> Pair (Const (f a)) (Identity (f a))) s
--    in (getConst x, runIdentity y)

view :: Lens s t a b -> s -> a
view l =
    getConst . l Const

over :: Lens s t a b -> (a->b) -> s -> t
over l f =
    runIdentity . l (Identity . f)

set :: Lens' s a -> a -> s -> s
set l =
    over l . const

-- Modify the target of a lens, but return the old value.
(<<%~) :: Lens s t a b -> (a -> b) -> s -> (a, t)
(<<%~) l f =
    l (fmap f . join (,))
    --l (\a -> (a, f a))

-- There's a () in every value. (No idea what this one is for, maybe it'll
-- become clear later.)
united :: Lens' s ()
united = lens (const ()) const
