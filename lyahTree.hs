-- Learn You A Haskell, ch 7, "Let's plant a tree"
--


data Tree a = EmptyTree | Node a (Tree a) (Tree a)
    deriving (Show, Eq)


treeInsert :: Ord a => a -> Tree a -> Tree a
treeInsert x EmptyTree = singleton x
treeInsert x (Node n left right)
    | x > n = Node n left (treeInsert x right)
    | x < n = Node n (treeInsert x left) right
    | x == n = Node x left right

singleton :: a -> Tree a
singleton a = Node a EmptyTree EmptyTree

treeElem :: Ord a => a -> Tree a -> Bool
treeElem _ EmptyTree = False
treeElem x (Node n left right)
    | x == n = True
    | x < n = treeElem x left
    | x > n = treeElem x right
{-
*Main> let nums = [8,6,4,1,7,3,5]
*Main> let tree = foldr treeInsert EmptyTree nums
 -}

-- Type classes 102

data TrafficLight = Red | Yellow | Green

instance Eq TrafficLight where
    Red == Red = True
    Yellow == Yellow = True
    Green == Green = True
    _ == _ = False
instance Show TrafficLight where
    show Red = "Red light"
    show Yellow = "Yellow light"
    show Green = "Green light"

-- A Yes-No Type Class
class YesNo a where
    yesno :: a -> Bool
instance YesNo Int where
    yesno 0 = False
    yesno _ = True
instance YesNo [a] where
    yesno [] = False
    yesno _ = True
instance YesNo Bool where
    yesno = id
instance YesNo (Maybe a) where
    yesno Nothing = False
    yesno _ = True
instance YesNo (Tree a) where
    yesno EmptyTree = False
    yesno _ = True
instance YesNo TrafficLight where
    yesno Red = False
    yesno _ = True

yesnoIf :: (YesNo y) => y -> a -> a -> a
yesnoIf y t f = if yesno y then t else f

-- Functors
instance Functor Tree where
    fmap f EmptyTree = EmptyTree
    fmap f (Node a left right) = Node (f a) (fmap f left) (fmap f right)

data MyMap k v = EmptyMap | MapEntry k v (MyMap k v) deriving Show
instance Functor (MyMap k) where
    fmap f EmptyMap = EmptyMap
    fmap f (MapEntry k v rest) = MapEntry k (f v) (fmap f rest)

    


