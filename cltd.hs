

sample = [ [0..2], [3..5], [6..8] ]

diffMatrix :: Num a => [[a]] -> [[a]]
diffMatrix = zipWith (diff) [1..]

diff :: Num a => Int -> [a] -> [a]
diff n xs = diffWithNext pre ++ [last pre] ++ diffWithPrev ((last pre):post)
    where
        (pre, post) = splitAt n xs
        diffWithNext as = zipWith (-) as (tail as)
        diffWithPrev as = zipWith (-) (tail as) as


--diffMatrix :: Num a => [[a]] -> [[a]]
--diffMatrix xss = map (\(n, xs) -> diff n xs) $ zip [1..] xss
--
--diff :: Num a => Int -> [a] -> [a]
--diff n xs = diffWithNext pre ++ [last pre] ++ diffWithPrev ((last pre):post)
--    where
--        (pre, post) = splitAt n xs
--        diffWithNext as = zipWith (\x y -> x-y) as (tail as)
--        diffWithPrev as = zipWith (\x y -> y-x) as (tail as)
