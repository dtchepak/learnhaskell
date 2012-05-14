
--splitWhere :: (a -> Bool) -> [a] -> [[a]]
--splitWhere _ Nil = Nil
--splitWhere _ [x] = [[x]]
--splitWhere p (x:xs) = if p x then [x]:splitWhere p xs else 
--splitWhere p [x,y] = if p x || p y then [[x],[y]] else [x:y:[]]
--splitWhere p x:y:z = if p x || p y then [[x],[y]] else [x:y:[]]
--splitWhere p xss = current:accumulated
--    where
--        (accumulated, current) = foldr (\x (acc,cur) -> if p x then ((merge x cur):acc,[]) else (acc,x:cur)) ([],[]) xss
--        merge x [] = [x]
--        merge x  z = x:z
--
--splitWhen :: (a -> Bool) -> [a] -> [[a]]
--splitWhen _ [] = []
--splitWhen p (x:y:xs) = if p x then _ else 
import Data.List (isInfixOf)

splitWhere :: [a] -> (a -> Bool) -> [[a]]
splitWhere xs p = combine [] rest lists
    where
        (lists, rest) = foldr (\x (l,r) -> if p x then (combine [x] r l,[]) else (l,x:r)) ([],[]) xs
        combine [] [] c = c
        combine [] b c = b:c
        combine a [] c = a:c
        combine a b c = a:b:c

split' :: [a] -> (a -> Bool) -> [[a]]
split' [] _ = []
split' (h:t) p = case split' t p of
    all@(x:xs) -> if p h then [h]:all else (h:x):xs
    []         -> [[h]]

split'' :: [a] -> (a->Bool) -> [[a]]
split'' xs p = case break p xs of
    ([],[]) -> []
    (l,[]) -> [l]
    (l,r:rs) -> l:[r]:split'' rs p

split''' :: [a] -> (a->Bool) -> [[a]]
split''' xs p = merge (break p xs)
    where 
        merge (l,[]) = [l]
        merge (l,r:rs) = l:[r]:split''' rs p
--
--    case recursiveCallGoesHere of  (rech:rect) -> (h:rech):rect  | [] -> [[h]] 
--    roconnor> davesq: okay suppose you have a list "hello|world|howdy||" and magically you also have the answer for "ello|world|howdy||" which is ["ello","|","world","|","howdy","|","|"].  how would you add 'h' to this answer to get the answer to your full input?
--saep> >  let f xs = case break (=='|') xs of { (l,[]) -> [l] ; (l,r:rs) -> l:[r]:f rs } in f "Hello|World|howdy||"

splitListAllAt :: String -> [[String]] -> [[[String]]]
splitListAllAt a (xss) = combine $ foldr(\xs (current, acc)-> if p xs then ([],(xs:current):acc) else (xs:current, acc)) ([],[]) xss
    where p xs = isInfixOf a (head xs)

combine ::  (a, [a]) -> [a]
combine (a,b) = a:b

--          split' (acc, [], []) = acc
--          split' (acc, cur, []) = cur:acc
  --          split' (acc, cur, (r:rest)) = if p r then ([r]:cur:acc, [], rest) else (acc, r:curr, rest)

shiftUntil :: ([a], [a]) -> (a -> Bool) -> ([a], [a])
shiftUntil (xs, ys) p = foldr (\y (l,r) -> if p y then (l,y:r) else (y:l,r)) (xs, []) ys

