module Brownbag where
import Prelude hiding (Left, Right)

data Direction = Left | Right | Straight deriving (Show, Eq)

dx :: Num a => (a, t) -> (a, t1) -> a
dx (ax,_) (bx,_) = bx - ax

dy :: Num a => (t, a) -> (t1, a) -> a
dy (_,ay) (_,by) = by - ay

direction :: (Ord a, Num a) => (a,a) -> (a,a) -> (a,a) -> Direction
direction a b c | p == 0 = Straight
                | p < 0  = Right
                | p > 0  = Left
    where p = (dx a b) * (dy a c) - (dy a b) * (dx a c)

directions :: (Ord a, Num a) => [(a,a)] -> [Direction]
directions (x:y:z:rest) = (direction x y z):(directions (y:z:rest))
directions _ = [] 

--grahamScan :: (Ord a, Num a) => [(a,a)] -> [(a,a)]
--
minY :: (Ord a) => [(a,a)] -> a
minY [] = error "asdf"
minY z = foldl1 pointWithMinY z
    where pointWithMinY a@(aX,aY) b@(bX,bY) = if (aY<bY) then a else b


{-
minY
sortByIncreasingAngle (cos increases when going from minY)

left turn - keep
right turn - throw
straight - throw
-}

