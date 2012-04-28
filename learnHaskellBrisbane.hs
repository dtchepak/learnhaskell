-- https://github.com/learnhaskell-brisbane/learn/wiki


-- ------------
--   WEEK 6
-- ------------
import Data.List

curry' :: ((a,b) -> c) -> a -> b -> c
curry' f = \a b -> f (a,b)
uncurry' :: (a -> b -> c) -> (a,b) -> c
uncurry' f = \(a,b) -> f a b

prob2 :: (a -> b) -> (a -> Bool) -> [a] -> [b]
prob2 f p = map f . filter p

length' :: [b] -> Integer
length' = foldl' (const.succ) 0
append' :: [a] -> [a] -> [a]
append' = flip (foldr (:)) --flip . foldr $ (:)
flatten' :: [[a]] -> [a]
flatten' = foldr append' []
flatmap' :: (a -> [b]) -> [a] -> [b]
flatmap' f = flatten' . map f

dec2nat :: [Int] -> Int
--dec2nat xs = snd $ foldr (\x (i,a) -> (succ i, 10^i * x + a)) (0,0) xs
dec2nat = foldl' (\a x -> 10*a + x) 0

--fib

prob6 = (+5) . (8/)

fst' :: (a, b) -> a
fst' = uncurry' const
snd' :: (a, b) -> b
snd' = uncurry' (flip const)

testProb1Curry   = (curry' fst) 11 22 == 11
testProb1Uncurry = (uncurry' (+)) (12,23) == 35
--testProb2        = prob2 (+11) odd [1..10] == [13,15,17,19,21] --< wrong answer?
testProb3Length  = length' [1..10] == 10
testProb3Append  = append' "has" "kell" == "haskell"
testProb3Flatten = flatten' ["abc","def","ghi"] == "abcdefghi"
testProb3Flatmap = flatmap' (replicate 3) [1..4] == [1,1,1,2,2,2,3,3,3,4,4,4]
testProb4        = dec2nat [2, 3, 4, 5] == 2345
--testProb5        = fib' 10 == [0,1,1,2,3,5,8,13,21,34,55]
testProb6        = prob6 2 == 9
testProb7Fst     = fst' (1,2) == 1
testProb7Snd     = snd' (1,2) == 2



