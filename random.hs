import System.Random

coins :: RandomGen g => g -> Int -> [Bool]
coins g n = take n $ randoms g
