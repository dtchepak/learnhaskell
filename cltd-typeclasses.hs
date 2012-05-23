
-- Problem 1
data NestedList a = Elem a | List [NestedList a] deriving (Show,Eq)

flatten :: NestedList a -> [a]
flatten (Elem a) = [a]
flatten (List []) = []
flatten (List (h:t)) = flatten h ++ flatten (List t)

-- Problem 2
-- 2.1
data Tree a = Empty | Node a (Tree a) (Tree a) deriving (Show, Eq)

-- 2.2
treeInsert :: Ord a => Tree a -> a -> Tree a
treeInsert Empty a = Node a Empty Empty
treeInsert tree@(Node x left right) a
    | x == a = tree
    | a < x  = Node x (left `treeInsert` a) right
    | a > x  = Node x left (right `treeInsert` a)

-- 2.3
treeFromList :: Ord a => [a] -> Tree a
treeFromList = foldr (flip treeInsert) Empty

-- Problem 3

class Mappable m where
    mmap :: (a -> b) -> m a -> m b

-- 3.1
instance Mappable NestedList where
    mmap f (Elem a) = Elem (f a)
    mmap _ (List []) = List []
    mmap f (List lists) = List $ map (mmap f) lists

-- 3.2
instance Mappable Tree where
    mmap _ Empty = Empty
    mmap f (Node a left right) = Node (f a) (mmap f left) (mmap f right)

instance Mappable [] where
    mmap = map

instance Mappable Maybe where
    mmap _ Nothing = Nothing
    mmap f (Just x) = Just (f x)
    
-- 3.3
increaseAll :: (Enum a, Mappable m) => m a -> m a
increaseAll = mmap succ

