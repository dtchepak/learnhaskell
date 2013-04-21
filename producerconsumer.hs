import Control.Applicative
import Control.Monad
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
    let loop i = Consume $ maybe (Done i) (\x -> if even x then loop (x+i) else Done i)
    in loop 0

takeWhileP :: (i -> Bool) -> Producer i -> Producer i
takeWhileP _ EndStream = EndStream
takeWhileP pred (Next a p) = if pred a then Next a (takeWhileP pred p) else EndStream

takeWhileC :: (i -> Bool) -> i -> Consumer i i
takeWhileC p i = Consume $ maybe (Done i) (\x -> if p x then takeWhileC p i else Done i)

sumC :: Consumer Integer Integer
sumC =
    let loop i = Consume $ maybe (Done i) (loop . (i+)) 
    in loop 0

adapt :: Consumer i o -> Consumer o o' -> Consumer i o'
adapt _ (Done o') = Done o'
adapt (Done o) (Consume c) = adapt (Done o) (Consume (const (c Nothing)))
adapt (Consume c) c' = Consume $ \mi -> adapt (c mi) c' -- <--- rong

getLineC :: Consumer Char String
getLineC =
    let loop s = Consume $ \mc -> case mc of
                                    Just '\n' -> Done (reverse s)
                                    Nothing   -> Done (reverse s)
                                    Just c    -> loop (c:s)
    in loop ""

instance Functor (Consumer i) where
    fmap f (Done o) = Done (f o)
    fmap f (Consume c) = Consume $ \ma -> fmap f (c ma)

joinConsumer :: Consumer i (Consumer i o) -> Consumer i o
joinConsumer (Done c) = c
joinConsumer (Consume c) = Consume $ \mi -> join (c mi)

instance Applicative (Consumer i) where
    f <*> a = f >>= \f' -> fmap f' a
    pure = return

instance Monad (Consumer i) where
    return = Done
    c >>= f = 
        let joinC :: Consumer i (Consumer i o) -> Consumer i o
            joinC (Done c) = c
            joinC (Consume c) = Consume $ joinC . c
        in joinC (fmap f c)
    --(Done o) >>= f = f o
    --(Consume c) >>= f = undefined --Consume (c >>> f)

getLinesC :: Consumer Char [String]
getLinesC = 
    let loop s = getLineC >>= \l -> case l of
                                        "" -> Done (reverse s)
                                        _  -> loop (l:s)
    in loop []


--sumUntilOdd' :: Consumer Integer Integer
--sumUntilOdd' = 
--blarg :: Producer Integer -> Consumer Integer Integer
--blarg EndStream =

--transform :: Producer i -> Consumer i o -> Producer o
--transform _ (Done o) = Next o EndStream
--transform EndStream (Consume c) = EndStream

tp x = trace (show x) x
sampleNum = produce [tp 2, tp 4, tp 6, tp 7, tp 8, tp 9]

-- eval (takeWhileP even sampleNum) sumC
