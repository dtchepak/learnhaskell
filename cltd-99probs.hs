
data TrafficLight = Red | Yellow | Green
    deriving Show

change :: TrafficLight -> TrafficLight
change Red = Green
change Yellow = Red
change Green = Yellow

data Vector a = Vector a a a deriving Show

data NestedList t = Elem t | List [NestedList t]
    deriving Show

-- List [] or
-- List [Elem 2]
-- List [Elem 3, List [Elem 5]]
--
flatten :: NestedList t -> [t]
flatten (Elem x) = [x]
flatten (List []) = []
flatten (List (h:t)) = flatten h ++ flatten (List t)

-- List [List t]
-- flatten :: List [List t] -> [[t]]
-- List [h:t]
-- t :: [List t]
-- flatten (List t) --> flatten (List [List t])
