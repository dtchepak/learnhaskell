foldLeft :: (a -> b -> a) -> a -> [b] -> a
foldLeft f acc (head:tail) = foldLeft f (f acc head) tail
foldLeft _ seed [] = seed

foldRight :: (a -> b -> b) -> b -> [a] -> b
foldRight f seed (head:tail) = f head (foldRight f seed tail)
foldRight _ seed [] = seed

mapFnLeft :: (a->b) -> [a] -> [b]
mapFnLeft fn list = foldLeft (\acc element -> acc ++ [fn element]) [] list

mapFnRight :: (a->b) -> [a] -> [b]
mapFnRight fn list = foldRight (\element acc -> (fn element) : acc) [] list


addOneRight :: Num a => [a] -> [a]
addOneRight list = foldRight (\head acc -> (head+1):acc) [] list

addOneLeft' :: Num a => [a] -> [a]
addOneLeft' list = foldLeft (\acc head -> (head+1):acc) [] list

addOneLeft :: Num a => [a] -> [a]
addOneLeft list = foldLeft (\acc head -> acc ++ [head+1]) [] list

take' :: Num a => a -> [b] -> [b]
take' 0 list = []
take' _ [] = []
take' i (head:tail) = head : (take' (i-1) tail)


any' :: (a -> Bool) -> [a] -> Bool
any' p list = foldRight (\h acc -> if p h then True else acc) False list

or' :: Bool -> Bool -> Bool
or' True _ = True
or' _ True = True
or' _ _ = False
