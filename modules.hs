import Data.List
import qualified Data.Map as Map


numUniques :: (Eq a) => [a] -> Int
numUniques = length . nub

phoneBook =   
    [("betty","555-2938")  
    ,("bonnie","452-2928")  
    ,("patsy","493-2928")  
    ,("lucille","205-2928")  
    ,("wendy","939-8282")  
    ,("penny","853-2492")  
    ]  

findByKey :: (Eq k) => k -> [(k,v)] ->  Maybe v
findByKey _ [] = Nothing
findByKey key ((k,v):xs)
    | key == k = Just v
    | otherwise = findByKey key xs

findByKey'' :: (Eq k) => k -> [(k,v)] ->  Maybe v
findByKey'' key = foldl (\acc (k,v) -> if key==k then Just v else acc) Nothing

findByKey''' :: (Eq k) => k -> [(k,v)] ->  [Maybe v]
findByKey''' key = scanl (\acc (k,v) -> if key==k then Just v else acc) Nothing

findByKey'''' :: (Eq k) => k -> [(k,v)] ->  [Maybe v]
findByKey'''' key = scanr (\(k,v) acc -> if key==k then Just v else acc) Nothing

findByKey' :: (Eq a) => a -> [(a,b)] ->  b
findByKey' key xs = snd . head . filter (\(k,v) -> key == k) $ xs  

phoneBookToMap :: (Ord k) => [(k, String)] -> Map.Map k String  
phoneBookToMap = Map.fromListWith (\number1 number2 -> number1 ++ ", " ++ number2) 








