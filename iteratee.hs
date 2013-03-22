import Control.Monad

data Iteratee i a = Done a
                  | Next (Maybe i -> Iteratee i a)

(>>>) :: Monad m => (a -> m b) -> (b -> m c) -> (a -> m c)
f >>> g = \a -> f a >>= g

-- (Maybe i -> Iteratee i a) -> (a -> Iteratee i b) -> (Maybe i -> Iteratee i b)
instance Monad (Iteratee i) where
    return = Done
    (Done a) >>= f = f a
    (Next ka) >>= f = Next (ka >>> f)

getLineI :: Iteratee Char String
getLineI =
    let loop :: String -> Iteratee Char String
        loop s = Next $ \mc -> case mc of
                                    Just '\n' -> Done (reverse s)
                                    Nothing   -> Done (reverse s)
                                    Just c    -> loop (c:s)
    in  loop ""

getLines :: Iteratee Char [String]
getLines =
    let loop acc = getLineI >>= \l -> case l of
                                        "" -> Done .reverse $ acc
                                        _  -> loop (l:acc)
    in loop []

getNum :: Iteratee Char (Maybe Int)
getNum = getLineI >>= \l -> case reads l of
                                [(a, [])] -> return (Just a)
                                _             -> return Nothing

revLines :: Iteratee Char [String]
revLines = getLines >>= return . reverse

eval :: String -> Iteratee Char a -> a
eval _ (Done x) = x
eval [] (Next x) = eval [] (x Nothing)
eval (c:cs) (Next x) = eval cs (x (Just c))

en_str "" i = i
en_str (c:t) (Next k) = en_str t $ k (Just c)
en_str _ (Done x) = Done x

run (Next k) = run $ k Nothing
run (Done x) = x

main = 
    let ls = "hello\nworld\n!"
    in do
        putStrLn ls
        putStrLn "-----------"
        putStrLn $ eval ls getLineI
        putStrLn . unwords $ eval ls getLines
        putStrLn . unwords $ eval ls revLines
