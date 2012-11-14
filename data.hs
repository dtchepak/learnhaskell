
data TrafficLight = Red | Yellow | Green
instance Show TrafficLight where
    show Red = "Red: STOP!"
    show Yellow = "Yellow: Slow down..."
    show Green = "Green: Go!"

change :: TrafficLight -> TrafficLight
change Red = Green
change Yellow = Red
change Green = Yellow

class Changeable a where
    next :: a -> a

instance Changeable TrafficLight where
    next = change

--data Tree a = Empty | Node a (Tree a) (Tree a) deriving (Show, Eq)
--
data Vector a = Vector a a a deriving (Show,Eq)
data Vector2 a = Vector2 {x :: a, y :: a, z :: a} deriving (Show,Eq)




