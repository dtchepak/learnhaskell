
main = do
    print $ take 10000000 $ forever 1

forever x = x : forever x
