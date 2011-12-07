import System.Random

map' :: (a -> b) -> [a] -> [b]
map' _ [] = []
map' f (x:xs) = f x : map' f xs

rollDie :: IO Int
rollDie = getStdRandom (randomR (1,6))

rollDice :: (RandomGen g) => Int -> g -> Int
rollDice 0 _ = 0
rollDice n generator =
    r + rollDice (n-1) newGenerator
    where (r, newGenerator) = randomR (1,6) generator


roll :: Int -> IO(Int)
roll n = do
    gen <- newStdGen
    return $ rollDice n gen


