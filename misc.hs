import System.Random

map' :: (a -> b) -> [a] -> [b]
map' _ [] = []
map' f (x:xs) = f x : map' f xs

rollDie :: IO Int
rollDie = getStdRandom (randomR (1,6))

rollDice :: (RandomGen g) => Int -> g -> Int
rollDice 0 _ = 0
rollDice n generator =
    r + rollDice (n-1) newGenerator
    where (r, newGenerator) = randomR (1,6) generator


roll :: Int -> IO(Int)
roll n = do
    gen <- newStdGen
    return $ rollDice n gen

sort' :: Ord a => [a] -> [a]
sort' [] = []
sort' (a:[]) = [a]
sort' l = minValue : sort' (except_first minValue l)
    where minValue = minimum l

except_first :: Eq a => a -> [a] -> [a]
except_first _ [] = []
except_first a (x:xs) = if (x==a) then xs else x:(except_first a xs)

unzip' :: [(a,b)] -> ([a], [b])
unzip' [] = ([],[])
unzip' ((a,b):rest) = (a:as, b:bs)
    where (as, bs) = unzip' rest
--unzip' ((a,b):rest) = (a:(fst (unzip' rest)), b:(snd (unzip' rest)))

factorial :: Int -> Int
factorial 1 = 1
factorial n = n * factorial (n-1)

-- Comb n k: From n items, pick k
comb :: [a] -> Int -> [[a]]
comb _ 0 = [[]]
comb (x:xs) k
    | n < k = []
    | n == k = [x:xs] 
    | otherwise = combs_of_x ++ combs_of_xs
    where
        n = 1 + length xs
        combs_of_x = map ([x]++) $ comb xs (k-1)
        combs_of_xs = comb xs k



