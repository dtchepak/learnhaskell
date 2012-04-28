
main = do
    print $ take 10000000 $ forever 1

forever x = zs
    where zs = x : zs
