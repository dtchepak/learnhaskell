
--data TakeState a = TakeState Int a deriving (Show) 
--take' n =
--    let f (TakeState i acc) head = if i==n then (TakeState i acc) else (TakeState (i+1) (acc++[head]))
--    in foldl f (TakeState 0 [])
--
--take' n list = 
--    let f _ (TakeState 0 acc) = (TakeState 0 acc)
--        f head (TakeState i acc) = (TakeState (i-1) (head : acc))
--    in foldr f (TakeState n []) list
--
--take2 n list =
--    let f head (TakeState i acc) = if i==n then (TakeState i acc) else (TakeState (i+1) (head:acc))
--    in foldr f (TakeState 0 []) list
--


foldRight :: (a -> b -> b) -> b -> [a] -> b
foldRight f s (h:t) = f h (foldRight f s t)
foldRight _ s [] = s

{-
take' 1 [1,2]
    =  f 1 (foldRight f (TakeState 1 []) [2])
    =  f 1 (f 2 (foldRight f (TakeState 1 []) []))
    =  f 1 (f 2 (TakeState 1 []))
    =  f 1 (TakeState 0 2:[])
    =  TakeState 0 [2]



take' 1 [1,2]
    = foldr f (TakeState 1 []) [1,2]
    = foldr f (f 1 (TakeState 1 [])) [2]
    = foldr f (TakeState 0 1:[]) [2]
    = foldr f (TakeState 0 [1]) [2]
    = foldr f (TakeState 0 [1]) []
-}
    

take'' :: Int -> [a] -> [a]
take'' 0 _     = []
take'' _ []    = []
take'' n (h:t) = h: take'' (n-1) t

--take' :: Int -> [a] -> [a]
--take' n list = 
--    fst ( foldr (\h (a,i) -> if i==n then (a,i) else ((a ++ [h]),(i+1))) ([],0) (reverse list) )


zip' :: [a] -> [b] -> [(a,b)]
zip' x y = foldr (\h a -> 
            foldr (\h2 b -> ((h,h2):b)) [] y
                ) [] x

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' f (a:as) (b:bs) = (f a b):(zipWith' f as bs)
zipWith' _ _ _ = []
--
--zipWith' f (a:as) bs = (f a (head bs)):(zipWith' f as (tail bs))
--
--
--zipWith'' f x y = fst $ foldr (\hx (acc,listy) -> ((f hx (head listy)):acc, (tail listy))) ([], reverse y) x
--
zipWith'' f a b = 
    let fn h (acc, (z:zs)) = ((f h z):acc, zs)
        fn h (acc, []) = (acc, [])
    in foldr fn ([], reverse b) a

zipWith''' f a b = 
    let fn h (acc, x:xs) = ((f h x):acc, xs)
        fn _ (acc, _)    = (acc, [])
    in foldr fn ([], reverse b) a

fold2r :: (a -> b -> c -> c) -> c -> [a] -> [b] -> c
fold2r f seed (a:as) (b:bs) = f a b (fold2r f seed as bs)
fold2r _ seed _ _ = seed

zipWithFold2r f = fold2r (\a b c -> (f a b):c) []

zipFold2r = fold2r (\a b c -> (a,b):c) []



    

