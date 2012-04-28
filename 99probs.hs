-- 1
myLast :: [a] -> a
myLast = foldr1 (flip const)


