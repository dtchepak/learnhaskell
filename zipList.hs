-- From http://en.wikibooks.org/wiki/Haskell/Applicative_Functors#ZLists
import Control.Applicative
newtype ZList a = ZList [a]
    deriving (Show, Eq)

instance Functor ZList where
  fmap f (ZList xs) = ZList (map f xs)

instance Applicative ZList where
  (ZList fs) <*> (ZList xs) = ZList (zipWith ($) fs xs)
  pure x                        = ZList (repeat x)
