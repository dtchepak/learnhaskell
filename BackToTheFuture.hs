import Control.Monad

newtype State s a = State { runState :: s -> (a,s) }

instance Functor (State s) where
    fmap f st = State (\s -> let (a,s') = runState st s
                             in (f a,s'))

instance Monad (State s) where
    return x = State (\s -> (x,s))
    st >>= f = State (\s ->
                let (a, s')   = runState st s
                    (a',s'') = runState (f a) s'
                in ( a',s''))


newtype Delorean s a = Delorean { runBack :: s -> (a,s) }
instance Functor (Delorean s) where
    fmap f st = Delorean (\s -> let (a',s') = runBack st s
                                in (f a',s'))

instance Monad (Delorean s) where
    return x = Delorean (\s -> (x,s))
    st >>= f = Delorean (\s ->
                let (a,s'') = runBack st s'
                    (a',s') = runBack (f a) s
                in (a',s'') )

newtype Stack a = Stack [a] deriving (Show, Eq)

push :: a -> Stack a -> ((), Stack a)
push x (Stack l) = ((), Stack (x:l))

pop :: Stack a -> (Maybe a, Stack a)
pop (Stack []) = (Nothing, Stack [])
pop (Stack (x:xs)) = (Just x, Stack xs)


-- GHCI HELPERS
--
push' a  = State (push a)
pop'     = State (pop)
push'' a = Delorean (push a)
pop''    = Delorean (pop)

{-
ghci> :r
[1 of 1] Compiling Main             ( BackToTheFuture.hs, interpreted )
Ok, modules loaded: Main.
ghci> runState (push' 1 >> push' 2 >> push' 3) (Stack [])
(Stack [3,2,1],())
ghci> runBack (push'' 1 >> push'' 2 >> push'' 3) (Stack [])
(Stack [1,2,3],())
ghci> runState (push' 1 >> push' 2 >> push' 3 >> pop' >> push' 4) (Stack [])
(Stack [4,2,1],())
ghci> runBack (push'' 1 >> push'' 2 >> push'' 3 >> pop'' >> push'' 4) (Stack [])
(Stack [1,2,3],())
ghci> runBack (push'' 1 >> push'' 2 >> push'' 3 >> pop'' >> push'' 4 >> pop'') (St
ck [])
(Stack [1,2,3],Nothing)
ghci> runBack (pop'' >> push'' 1 >> push'' 2 >> push'' 3 >> pop'' >> push'' 4) (St
ck [])
(Stack [2,3],())
ghci> runState (pop' >> push' 1 >> push' 2 >> push' 3 >> pop' >> push' 4) (Stack [
)
(Stack [4,2,1],())
 -}



