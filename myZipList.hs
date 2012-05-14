import Control.Applicative

--data MyZipList a = MyZipList [a] deriving (Show,Eq)

newtype MyZipList a = MyZipList { getMyZipList :: [a] }
    deriving (Show,Eq)

instance Functor MyZipList where
    fmap f (MyZipList a) = (MyZipList (map f a))


instance Applicative MyZipList where
    --pure a = MyZipList [a]
    pure a = MyZipList (repeat a)
    (MyZipList fs) <*> (MyZipList as) = (MyZipList (zipWith ($) fs as))


