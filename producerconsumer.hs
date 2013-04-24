import Control.Applicative
import Control.Monad
import Data.Maybe 
import Debug.Trace

data Producer a = EndStream
                | Next a (Producer a)

instance Functor Producer where
    fmap f EndStream = EndStream
    fmap f (Next a p) = Next (f a) (fmap f p)

produce :: [a] -> Producer a
produce [] = EndStream
produce (a:as) = Next a (produce as)

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

countI :: Consumer a Integer
countI = 
  let loop :: Integer -> Consumer a Integer
      loop acc = Consume $ maybe (Done acc) (const . loop . succ $ acc)
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


toListC :: Consumer a [a]
toListC = 
    let loop :: [a] -> Consumer a [a]
        loop acc = Consume $ maybe (Done . reverse $ acc) (\x -> loop (x:acc))
    in loop []

tp x = trace (show x) x
sampleNum = produce [tp 2, tp 4, tp 6, tp 7, tp 8, tp 9]

-- eval (takeWhileP even sampleNum) sumC
