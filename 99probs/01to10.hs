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


