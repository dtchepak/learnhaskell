zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys

flip' :: (a -> b -> c) -> (b -> a -> c)
flip' f a b = f b a

map' :: (a -> b) -> [a] -> [b]
map' _ [] = []
map' f (x:xs) = f x : map' f xs

filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter' predicate (x:xs)
    | predicate x = x : remainder
    | otherwise = remainder
    where remainder = filter' predicate xs

takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' _ [] = []
takeWhile' predicate (x:xs)
    | predicate x = x : takeWhile' predicate xs
    | otherwise = []


collatz :: (Integral a) => a -> [a]
collatz 1 = [1]
collatz x
    | even x = x : collatz (x `div` 2)
    | odd x = x : collatz ((x*3)+1)

numLongChains :: Int  
numLongChains = length (filter (\xs -> length xs > 15) (map collatz [1..100]))  

foldl' :: (a -> b -> a) -> a -> [b] -> a
foldl' _ acc [] = acc
foldl' f acc (x:xs) = foldl' f (f acc x) xs

foldr' :: (a -> b -> b) -> b -> [a] -> b
foldr' _ acc [] = acc
foldr' f acc (x:xs) = f x (foldr' f acc xs)


elem' :: (Eq a) => a -> [a] -> Bool
elem' i xs = foldl (\x y -> x || y == i) False xs


rev' :: [a] -> [a]
rev' = foldl (flip (:)) []

map2' :: (a -> b) -> [a] -> [b]
map2' f xs = foldr (\x acc -> f x : acc) [] xs


