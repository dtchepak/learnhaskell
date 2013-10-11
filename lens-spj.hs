-- from Haskell Exchange 2013, SPJ lens talk
-- http://skillsmatter.com/podcast/scala/lenses-compositional-data-access-and-manipulation/te-8510

{-# LANGUAGE RankNTypes #-}

import Data.Functor.Identity

data LensR s a = LensR { viewR :: s -> a
                       , setR :: a -> s -> s
                       }

type Lens' s a = forall f. Functor f => (a -> f a) -> s -> f s

view' :: Lens' s a -> s -> a
view' l = fst . l (\a -> (a,a)) -- SPJ used Const functor instead of (,) a
                                -- Const is newtype; cheaper?

--set' :: Lens' s a -> (a -> s -> s)
--set' l = \a -> runIdentity . l (Identity . const a)
set' :: Lens' s a -> a -> s -> s
set' l = over' l . const

over' :: Lens' s a -> (a->a) -> s -> s
over' l f = runIdentity . l (Identity . f)


-----------
data Person = Person { _name :: String, _salary :: Int }
    deriving (Show, Eq)

name :: Lens' Person String
name u (Person n s) = 
    fmap (\n' -> Person n' s) (u n)
