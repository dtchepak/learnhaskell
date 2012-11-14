data Tree a = Empty | Node a (Tree a) (Tree a) deriving (Show)
freeTree :: Tree Char 
freeTree =
    Node 'P'
        (Node 'O'
            (Node 'L'
                (Node 'N' Empty Empty)
                (Node 'T' Empty Empty)
            )
            (Node 'Y'
                (Node 'S' Empty Empty)
                (Node 'A' Empty Empty)
            )
        )
        (Node 'L'
            (Node 'W'
                (Node 'C' Empty Empty)
                (Node 'R' Empty Empty)
            )
            (Node 'A'
                (Node 'A' Empty Empty)
                (Node 'C' Empty Empty)
            )
        )


data Direction = L | R deriving (Show)
type Directions = [Direction]

changeToP :: Directions -> Tree Char -> Tree Char
changeToP (L:ds) (Node n l r) = Node n (changeToP ds l) r
changeToP (R:ds) (Node n l r) = Node n l (changeToP ds r)
changeToP [] (Node n l r) = Node 'P' l r

elemAt :: Directions -> Tree a -> a
elemAt [] (Node x _ _) = x
elemAt (L:ds) (Node _ l _) = elemAt ds l
elemAt (R:ds) (Node _ _ r) = elemAt ds r

data Crumb a = LeftCrumb a (Tree a) | RightCrumb a (Tree a) deriving (Show)
type Breadcrumbs a = [Crumb a]
type Zipper a = (Tree a, Breadcrumbs a) -- The part of data structure we're *focusing* on.

goLeft :: (Zipper a) -> (Zipper a)
goLeft (Node x l r, bs) = (l, LeftCrumb x r:bs)
goRight :: (Zipper a) -> (Zipper a)
goRight (Node x l r, bs) = (r, RightCrumb x l:bs)
goUp :: (Zipper a) -> (Zipper a)
goUp (l, (LeftCrumb x r):bs) = (Node x l r, bs)
goUp (r, (RightCrumb x l):bs) = (Node x l r, bs)

modify :: (a -> a) -> Zipper a -> Zipper a
modify f (Node x l r, bs) = (Node (f x) l r, bs)
modify f (Empty, bs) = (Empty, bs)


{-
 - TREE
 -             Node
 -            /    \
 -          Left   Right
 -
 - LIST
 -
 -   Node --> Child
 -
 - List can be thought of as tree with single branch.
 - Zippers can work for lists too.
 -}

-- BC for list, (List c, NodesAbove)
type ListZipper a = ([a], [a]) --Current list pos, prev elements

goForward :: ListZipper a -> ListZipper a
goForward (x:xs, bs) = (xs, x:bs)
goBackward :: ListZipper a -> ListZipper a
goBackward (xs, b:bs) = (b:xs, bs)
