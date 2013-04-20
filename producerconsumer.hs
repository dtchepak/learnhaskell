import Data.Maybe 
import Debug.Trace

data Producer a = EndStream
                | Next a (Producer a)


produce :: [a] -> Producer a
produce [] = EndStream
produce (a:as) = Next a (produce as)

--consume :: Producer a -> (a -> b) -> [b]
--consume EndStream _ = []
--consume (Next a p) f = (f a):consume p f
--
--consumeWhile :: (a -> Bool) -> Producer a -> [a]
--consumeWhile _ EndStream = []
--consumeWhile pred (Next a p) = if pred a then a:consumeWhile pred p
--                               else []
--
--consume' :: Producer a -> (b -> a -> b) -> b -> b
--consume' EndStream _ b = b
--consume' (Next a p) f b = 
--    let b' = f b a
--    in b' `seq` consume' p f b'

consume :: Producer a -> (b -> a -> Maybe b) -> b -> b
consume EndStream _ b = b
consume (Next a p) f b =
    let b' = f b a
    in maybe b (consume p f) b'

data Consumer i o = Done o
                  | Consume (Maybe i -> Consumer i o)

eval :: Producer a -> Consumer a o -> o
eval _ (Done x) = x
eval EndStream (Consume c) = eval EndStream (c Nothing)
eval (Next a p) (Consume c) = eval p (c (Just a))

sumUntilOdd :: Consumer Integer Integer
sumUntilOdd = 
    let loop i = Consume $ \x -> case x of
                                Nothing -> Done i
                                Just x' -> if even x' then loop (x' + i) else Done i
    in loop 0

takeWhileP :: (i -> Bool) -> Producer i -> Producer i
takeWhileP _ EndStream = EndStream
takeWhileP pred (Next a p) = if pred a then Next a (takeWhileP pred p) else EndStream

sumC :: Consumer Integer Integer
sumC =
    let loop i = Consume $ maybe (Done i) (loop . (i+)) 
    in loop 0

--sumUntilOdd' :: Consumer Integer Integer
--sumUntilOdd' = 

--transform :: Consumer i o -> Producer o
--transform (Done o) = Next o EndStream
--transform (Consume c) =
--
--
tp x = trace (show x) x
sampleNum = produce [tp 2, tp 4, tp 6, tp 7, tp 8, tp 9]

-- eval (takeWhileP even sampleNum) sumC
--
