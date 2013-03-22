import Data.Maybe
import Control.Monad

takeUntilBlank :: [Maybe a] -> [a]
--1)
--takeUntilBlank [] = []
--takeUntilBlank (Nothing:_) = []
--takeUntilBlank (x:xs) = maybe [] (\a -> a:takeUntilBlank xs) x
--
--2)
takeUntilBlank = map fromJust . takeUntil isNothing


takeUntil :: (a -> Bool) -> [a] -> [a]
--1)
-- takeUntil _ [] = []
-- takeUntil p (x:xs) = if p x then [] else x:takeUntil p xs
-- 2)
-- takeUntil p = foldr (\x acc -> if p x then [] else x:acc) []
-- 3)
takeUntil p = foldr (\x -> if p x then const [] else (x:)) []


