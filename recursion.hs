maximum' :: (Ord a) => [a] -> a
maximum' [] = error "No maximum for empty list"
maximum' [x] = x
maximum' (x:y) = max x (maximum' y)

replicate' :: (Num a) => Int -> a -> [a]
replicate' 0 _ = []
replicate' count num = num:replicate' (count-1) num

take' :: Int -> [a] -> [a]
take' 0 _ = []
take' _ [] = []
take' n (x:xs) = x:take' (n-1) xs

reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x]

zip' :: [a] -> [b] -> [(a,b)]
zip' [] _ = []
zip' _ [] = []
zip' (x:xs) (y:ys) = (x,y):zip' xs ys

elem' :: (Eq a) => a -> [a] -> Bool
elem' _ [] = False
elem' i (x:xs)
    | x == i = True
    | otherwise = elem' i xs

quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) = smaller ++ [x] ++ larger
    where smaller = quicksort [y | y <- xs, y <= x]
          larger = quicksort [y | y <- xs, y > x]




