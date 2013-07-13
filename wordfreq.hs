-- http://scottchamberlin.tumblr.com/post/55152416452/linqinterview
-- "Return the top 10 most frequently occurring words in a string"
{-# LANGUAGE TupleSections #-}
import Control.Monad.State
import Data.List as L
import Data.Map as M
import Data.Ord
import Data.Traversable

type Freq a = [(a,Int)]

freq :: Ord a => [a] -> Freq a
freq = M.toList . L.foldl' (\acc x -> insertWith (+) x 1 acc) M.empty

sortByFreq :: Ord a => [a] -> [a]
sortByFreq = fmap fst . sortBy (comparing (Down . snd)) . freq

topN :: Int -> String -> [String]
topN n = take n . sortByFreq . words

-- ruby golf (where `string` is instance of string to use):
-- string.split(' ').inject(Hash.new(0)) {|acc, new| acc[new]+=1; acc}.sort_by {|k,v| -v}

golf = sortBy (comparing (Down . snd)) . M.toList . fromListWith (+) . fmap (,1) . words


-- traverse with state monad
freq' :: (Ord a, Traversable t) => t a -> Freq a
freq' = 
    let updateState a = modify (insertWith (+) a 1)
        run = flip execState M.empty
    in M.toList . run . traverse updateState
-- takes longer than freq to process sample

-- SAMPLE
sample = readFile "../davesquared.net/source/_posts/2013-03-11-reasoning-and-mutability.pandoc"
