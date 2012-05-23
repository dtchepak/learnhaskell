import Data.List (foldl', group)
import Control.Applicative

--------------------------------------------
-- 1
myLast :: [a] -> a
myLast = foldr1 (flip const)

--------------------------------------------
-- 2
myButLast :: [a] -> a
myButLast [x,_] = x
myButLast (_:t) = myButLast t
myButLast _ = error "No second last element"

--------------------------------------------
-- 3
elementAt :: [a] -> Int -> a
[] `elementAt` _ = error "No such element"
(x:xs) `elementAt` i
    | i == 1    = x
    | i < 1     = error "Invalid index"
    | otherwise = xs `elementAt` (i-1)

--------------------------------------------
-- 4
myLength :: [a] -> Int
myLength = foldl' (const . succ) 0

--------------------------------------------
-- 5
reverse' :: [a] -> [a]
reverse' = foldl' (flip (:)) []

--------------------------------------------
-- 6
isPalindrome :: Eq a => [a] -> Bool
isPalindrome a = a == reverse' a

isPalindrome' :: Eq a => [a] -> Bool
isPalindrome' = (==) <$> id <*> reverse'

--------------------------------------------
-- 7
data List a = List [List a] | Elem a deriving (Show, Eq)

flatten' :: List a -> [a]
flatten' (Elem x) = [x]
flatten' (List []) = []
flatten' (List (h:t)) = flatten' h ++ flatten' (List t)

flatten'' :: List a -> [a]
flatten'' xs = flat xs []
    where
        flat (Elem x) a = x:a
        flat (List (h:t)) a = flat h (flat (List t) a)
        flat (List []) a = a

--------------------------------------------
-- 8
compress :: Eq a => [a] -> [a]
compress [] = []
compress [x] = [x]
compress (x:ys@(y:_)) = if x == y then compress ys else x:(compress ys)

compress' :: Eq a => [a] -> [a]
compress' = map head . group

compress'' :: Eq a => [a] -> [a]
compress'' [] = []
compress'' (h:t) = h:(compress'' $ dropWhile (==h) t)

--------------------------------------------
-- 9
pack :: Eq a => [a] -> [[a]]
pack = group

pack' :: Eq a => [a] -> [[a]]
pack' [] = []
pack' (x:xs) = case packed of
    [] -> [[x]]
    acc@(h:t) -> if x==head h then (x:h):t else [x]:acc
    where packed = pack' xs

pack'' :: Eq a => [a] -> [[a]]
pack'' = foldr packed []
    where packed x [] = [[x]]
          packed x acc@(y:ys) = if x == head y then (x:y):ys else [x]:acc

--------------------------------------------
-- 10
encode :: Eq a => [a] -> [(Int,a)]
encode = map (\x -> (myLength x, head x)) . pack

--------------------------------------------
-- 11
data RunLength a = Single a | Multiple Int a deriving (Show, Eq)
encodeModified :: Eq a => [a] -> [RunLength a]
encodeModified = map runLength . group
    where runLength [x] = Single x
          runLength xs@(x:_) = Multiple (length xs) x

--------------------------------------------
-- 12
decodeModified :: [RunLength a] -> [a]
decodeModified = foldr decode []
    where decode (Single x) = (x:)
          decode (Multiple n x) = (++) (replicate n x)

decodeModified' :: [RunLength a] -> [a]
decodeModified' = concatMap decode
    where decode (Single x) = [x]
          decode (Multiple n x) = replicate n x

--------------------------------------------
-- 13
encodeDirect :: Eq a => [a] -> [RunLength a]
encodeDirect = foldr encode []
    where
        encode x [] = [Single x]
        encode x rest@((Single a):_) = if x==a then (Multiple 2 x):rest else (Single x):rest
        encode x rest@((Multiple n a):_) = if x==a then (Multiple (n+1) x):rest else (Single x):rest

--------------------------------------------
-- 14
dupli :: [a] -> [a]
dupli = foldr (\x acc -> x:x:acc) []

--------------------------------------------
-- 15
repli :: [a] -> Int -> [a]
--repli xs n = concatMap (replicate n) xs   -- concatMap
--repli xs n = (concatMap . replicate) n xs
--repli = flip (concatMap . replicate)      -- pointfree
repli xs n = xs >>= replicate n             -- using list monad

--------------------------------------------
-- 16
dropEvery :: [a] -> Int -> [a]
dropEvery xs n = map snd $ filter (\(i,_) -> i `mod` n /= 0) $ zip [1..] xs

--------------------------------------------
-- 17
split :: [a] -> Int -> ([a],[a])
split xs n = (take n xs, drop n xs)

split' :: [a] -> Int -> ([a],[a])
split' xs 0 = ([], xs)
split' [] _ = ([], [])
split' (x:xs) n = let (l,r) = split' xs (n-1) in (x:l, r)

--------------------------------------------
-- 18
slice :: [a] -> Int -> Int -> [a]
slice xs i k = take (k-i+1) . drop (i-1) $ xs

slice' :: [a] -> Int -> Int -> [a]
slice' xs i k = let (l,_) = split xs k
                in drop (i-1) l

--------------------------------------------
-- 19
rotate :: [a] -> Int -> [a]
rotate xs 0 = xs    -- this case lets rotate _ 0 work on infinite lists (with take for eg)
rotate xs n = r ++ l
    where
        (l,r) = split xs index
        index
            | n > 0 = n                         -- fine with inf lists.
            | otherwise = length xs + n         -- not fine with inf lists.


--------------------------------------------
-- 20
removeAt :: Int -> [a] -> (a, [a])
removeAt n xs = (last l, init l ++ r)
    where (l,r) = split xs n

