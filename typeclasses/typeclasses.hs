{-# LANGUAGE NoImplicitPrelude, MultiParamTypeClasses #-}

import Prelude (flip, ($), Int, String, Char)

class Functor f where
    (<$>) :: (a -> b) -> (f a -> f b)

class Functor f => Apply f where
    (<*>) :: f (a -> b) -> (f a -> f b)

class Apply f => Bind f where
    (=<<) :: (a -> f b) -> (f a -> f b)
    (>>=) :: f a -> (a -> f b) -> f b
    (=<<) = flip (>>=)
    (>>=) = flip (=<<)

class Apply f => Applicative f where
    pure :: a -> f a

class (Bind f, Applicative f) => Monad f

class Functor f => Alt f where
    (<|>) :: f a -> f a -> f a

class Alt f => Plus f where
    zero :: f a

class (Plus f, Applicative f) => Alternative f

class (Alternative f, Monad f) => MonadPlus f

class Semigroupoid g where
    (.) :: g b c -> g a b -> g a c

class Semigroupoid g => Category g where
    id :: g a a

class Category g => Arrow g where
    arr :: (a -> b) -> g a b

newtype State s a = State { runState :: s -> (s,a) }
instance Functor (State s) where
    f <$> s = State $ \st -> 
                let (st', a') = runState s st
                in (st', f a')
instance Apply (State s) where
    f <*> s = State $ \st ->
                let (st', f') = runState f st
                    (st'', a) = runState s st'
                in (st'', f' a)
instance Bind (State s) where
--    f =<< s = State $ \st ->
--               let (st', a) = runState s st


