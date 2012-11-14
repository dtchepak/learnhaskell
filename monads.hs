import Control.Monad.Writer
import Control.Monad.State
import System.Random
import Data.List (group)

--
--instance Monad Maybe where
--    return x = Just x
--    Nothing (>>=) _ = Nothing
--    (Just x) (>>=) f = f x
--    fail _ = Nothing
--
type Birds = Int
type Pole = (Birds,Birds)

landLeft :: Birds -> Pole -> Maybe Pole
landLeft n (left, right)
    | abs ((left+n) - right) < 4 = Just (left+n, right)
    | otherwise = Nothing

landRight :: Birds -> Pole -> Maybe Pole
landRight n (left, right)
    | abs (left - (right + n)) < 4 = Just (left, right+n)
    | otherwise = Nothing

banana :: Pole -> Maybe Pole
banana _ = Nothing

routine :: Maybe Pole
routine = do
    start <- return (0,0)
    first <- landLeft 2 start
    second <- landRight 2 first
    landLeft 1 second

routine' :: Maybe Pole
routine' = return (0,0) >>= landLeft 2 >>= landRight 2 >>= landLeft 1

routine'' :: Maybe Pole
routine'' = return (0,0) >>= \x -> landLeft 2 x >>= (\y -> landRight 2 y) >>= landLeft 1


foo = (Just 3) >>= (\x -> Just "!" >>= (\y -> Just (show x ++ y)))
foo' = do
     x <- Just 3
     y <- Just "!"
     Just (show x ++ y)

--instance Monad [] where
--    return x = [x]
--    a >>= b = concatMap b a
--    fail _ = []
--[a] -> (a -> [b]) -> [b]
--
--[1,2] >>= \n -> ['a','b'] >>= \ch -> return (n, ch)
-- = concatMap (\n -> concatMap (\ch -> [(n,ch)]) ['a','b']) [1,2]
-- = concatMap (\n -> [(n,'a'), (n,'b')]) [1,2]
-- = [(1,'a'), (1,'b'), (2,'a'), (2,'b')]

logNumber :: Int -> Writer [String] Int
logNumber x = writer (x, ["Got number: " ++ show x])

multWithLog :: Writer [String] Int
multWithLog = do
    a <- logNumber 3
    b <- logNumber 5
    tell ["Gonna multiply these two"]
    return (a*b)


addStuff :: Int -> Int
addStuff = do
    a <- (*2)
    b <- (+10)
    return (a+b)

-- State monads

type Stack = [Int]
pop' :: Stack -> (Int, Stack)
pop' [] = undefined
pop' (x:xs) = (x, xs)
push' :: Int -> Stack -> ((), Stack)
push' x xs = ((), x:xs)

--(>>=) :: m a -> (a -> m b) -> m b
--(>>=) :: State s a -> (a -> State s b) -> State s b
--State { runState :: s->(a,s) }
newtype State' s a = State' { runState' :: s -> (a,s) }

instance Monad (State' s) where
    return x = State' (\s -> (x,s))
    state >>= f = State' $ \s -> 
                    let (a,s') = runState' state s
                    in runState' (f a) s'

pop :: State Stack Int
pop = state $ \(x:xs) -> (x, xs)
push :: Int -> State Stack ()
push x = state $ \s -> ((), x:s)


class Moveable a where
  move :: Moveable b => a -> b

doStackStuff :: State Stack Int
doStackStuff = do
    push 10
    push 20
    x <- pop
    push 30
    y <- pop
    return (x+y)
-- ghci> runState doStackStuff [1,2,3]

stackStuff :: State Stack ()
stackStuff = do
    x <- pop
    if x==5 
        then push x 
        else do
            push 3
            push 8

-- Random using State
randomSt :: (RandomGen g, Random a) => State g a
randomSt = state random

randomSt' :: (RandomGen g, Random a) => (a,a) -> State g a
randomSt' range = state $ randomR range

roll3 :: State StdGen (Int,Int,Int)
roll3 = do
    first <- randomSt' (1,6)
    second <- randomSt' (1,6)
    third <- randomSt' (1,6)
    return (first, second, third)


--instance (Error e) => Monad (Either e) where
--    return = Right
--    (Right r) >>= f = f r
--    (Left l) >>= _ = Left l
--    fail msg = Left (strMsg msg)
liftM' :: (Monad m) => (a -> b) -> m a -> m b
liftM' f ma = ma >>= (\a -> return (f a))
--liftM' f ma = ma >>= (return . f)
ap' :: (Monad m) => m (a -> b) -> m a -> m b
ap' f ma = f >>= (\f' -> 
            ma >>= (\a ->
            return (f' a) ))

ap'' :: (Monad m) => m (a -> b) -> m a -> m b
ap'' f ma = do
    f' <- f
    a <- ma
    return $ f' a

join' :: (Monad m) => m (m a) -> m a
join' m = m >>= id

data Segment x = Single x | Multiple Int x
    deriving (Show, Eq)
    
encodeModified :: (Eq a) => [a] -> [Segment a]

--encodeModified xs = map (\charset -> 
--                          if ((length charset) == 1) 
--                            then (Single (head charset)) 
--                            else (Multiple (length charset) (head charset))) (group xs)

encodeModified = map func . group
                    where func = (\charset -> 
                                    if ((length charset) == 1) 
                                      then (Single (head charset))
                                      else (Multiple (length charset) (head charset)))

