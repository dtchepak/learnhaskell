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


cataLength = foldr f 0
    where f a n = 1 + n

cataFilter p = foldr f []
    where f a as = if p a then (a:as) else as

unfold :: (b -> Bool) -> (b -> (a,b)) -> b -> [a]
unfold p g b = if p b then [] else a:(unfold p g b')
    where (a, b') = g b

--zip' :: [a] -> [b] -> [(a,b)]
--zip' = unfold p g
--    where p (as, bs) = as == [] || bs == []
--          g ((a:as), (b:bs)) = ((a,b),(as,bs))

windowed :: Int -> [a] -> [[a]]
windowed size [] = []
windowed size ls@(x:xs) = if length ls >= size then (take size ls) : windowed size xs else windowed size xs
--windowed size ls@(x:xs) = (take size ls) : windowed size xs

--windowed' :: Int -> [a] -> [[a]]
--windows' size list = fst $ foldr (\h (x,y) -> 
--windowed' size list = fst $ foldr (\h ((window,windows), acc) -> if length window == size-1 then (([], h:window ++ windows, acc) (([],[]),[]) list

--windowed' :: Int -> [a] -> [[a]]
--windowed' size list = snd $ foldr (\h (res, acc) -> if length res == size then ([], [res] ++ acc) else (h:res, acc)) ([], []) list
--
--


args f defaultA defaultB (a:as) (b:bs) = f a b : args f defaultA defaultB as bs
args f defaultA defaultB (a:as) _ = f a defaultB : args f defaultA defaultB as []
args f defaultA defaultB _ (b:bs) = f defaultA b : args f defaultA defaultB [] bs
args _ _ _ _ _ = []


