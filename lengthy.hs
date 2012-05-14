import Data.List
length' :: [a] -> Int
length' = foldl' (const' . succ) 0 

const' :: a -> b -> a
const' first second = first

