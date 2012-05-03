data Tuple a b = Tuple a b
data HardA a = HardA (a Int)
data HardB a b c = HardB (a b) (c a Int)

foldLeft :: (b -> a -> b) -> b -> [a] -> b
foldLeft f a [] = a
foldLeft f a (x:xs) = foldLeft f (f a x) xs

foldRight :: (a -> b -> b) -> b -> [a] -> b
foldRight f a [] = a
foldRight f a (x:xs) = f x (foldRight f a xs)

scanLeft :: (b -> a -> b) -> b -> [a] -> [b]
scanLeft f a [] = [a]
scanLeft f a (x:xs) =  a : scanLeft f (f a x) xs

scanRight :: (a -> b -> b) -> b -> [a] -> [b]
scanRight f a [] = [a]
scanRight f a (x:xs) = f x newAcc : scans
    where scans@(newAcc:_) = scanRight f a xs

map' :: (a -> b) -> [a] -> [b]
map' f = foldRight ((:) . f) []

findKey :: (Eq k) => k -> [(k,v)] -> Maybe v
findKey key = foldr (\(k,v) a -> if key == k then Just v else a) Nothing

-- LYAH Chapter 7
data Vector a = Vector a a a deriving (Show)
vplus :: Num a => Vector a -> Vector a -> Vector a
(Vector a b c) `vplus` (Vector x y z) = Vector (a+x) (b+y) (c+z)
dotProd :: (Num a) => Vector a -> Vector a -> a
(Vector i j k) `dotProd` (Vector l m n) = i*l + j*m + k*n
vmult :: (Num a) => Vector a -> a -> Vector a
(Vector i j k) `vmult` m = Vector (i*m) (j*m) (k*m)

data MyList a = Empty | a :-: (MyList a) deriving (Show, Read, Eq, Ord)


-- IO tests
putStr' :: String -> IO ()
putStr' = foldr (\x a -> (do
    putChar x
    a
    )) (return ())
    

gherkin :: (->) Int ((->) Int Int)
gherkin = (+)


addOne :: Int -> Int
addOne a = 1+a



doubleInput = do
    input <- getLine
    let enteredNumber = read input :: Int
    let double = 2 * enteredNumber
    return $ show double

doubleInputWithFmap = fmap (show . (2*). read) getLine

    
    












