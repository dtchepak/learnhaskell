-- http://scottchamberlin.tumblr.com/post/55152416452/linqinterview
-- "Return the top 10 most frequently occurring words in a string"
{-# LANGUAGE TupleSections #-}

import Data.List as L
import Data.Map as M
import Data.Ord

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


-- TODO: traverse with state monad?

-- SAMPLE
sample = readFile "../davesquared.net/source/_posts/2013-03-11-reasoning-and-mutability.pandoc"
