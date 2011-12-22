import Data.List

foldLeft :: (a -> b -> a) -> a -> [b] -> a
foldLeft _ accum [] = accum
foldLeft f accum (head:tail) = foldLeft f (f accum head) tail

foldLeft1 :: (a -> b -> a) -> a -> [b] -> a
foldLeft1 _ accum [] = accum
foldLeft1 f accum (head:tail) = 
    let new_accum = f accum head in
    foldLeft1 f new_accum tail


foldLeft2 :: (a -> b -> a) -> a -> [b] -> a
foldLeft2 _ accum [] = accum
foldLeft2 f accum (head:tail) = 
    let new_accum = f accum head in
    new_accum `seq` foldLeft2 f new_accum tail

foldRight :: (a -> b -> b) -> b -> [a] -> b
foldRight _ seed [] = seed
foldRight f seed (head:tail) = f head (foldRight f seed tail)

main = do
    let result = foldl' (+) 0 [1..100000]
    print result






