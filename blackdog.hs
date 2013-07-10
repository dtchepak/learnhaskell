import Data.Maybe
import Control.Monad (join)
import Control.Applicative ((<$>))

eg :: [IO (Maybe Int)]
eg = [return Nothing, return $ Just 1, putStrLn "oops" >> return Nothing]

firstSuccess :: [IO (Maybe a)] -> IO (Maybe a)
firstSuccess = foldr (\x acc -> do
                            res <- x
                            case res of
                                Nothing -> acc
                                a  -> return a) (return Nothing)

firstSuccess' :: [IO (Maybe a)] -> IO (Maybe a)
firstSuccess' [] = return Nothing
firstSuccess' (x:xs) = do
    res <- x
    case res of
            Nothing -> firstSuccess' xs
            a       -> return a

data Consumer i o = C { consume :: Maybe i -> Result i o }
data Result i o =
    Done o
    | Cont (Consumer i o)
instance Show o => Show (Result i o) where
    show (Done o) = show o
    show (Cont _) = "Cont"

iter :: [IO a] -> Consumer a b -> IO b
iter [] (C c) = let next = c Nothing
            in case next of
                Cont _ -> error "iter diverges"
                Done x -> return x
iter (x:xs) (C c) = 
    let step (Done a) = return a
        step (Cont c) = iter xs c
    in x >>= step . c . Just

firstOr :: a -> Consumer a a
firstOr x = C $ maybe (Done x) Done

firstJust :: Consumer (Maybe a) (Maybe a)
firstJust = 
    let result (Just (Just x)) = Done (Just x)
        result (Just _)        = Cont firstJust
        result Nothing         = Done Nothing
    in C result

findI :: (a -> Bool) -> Consumer a (Maybe a)
findI p =
    let result (Just x) = if p x then Done (Just x) else Cont (findI p)
        result Nothing  = Done Nothing
    in C result

instance Functor (Result i) where
    fmap f (Done o) = Done $ f o
    fmap f (Cont c) = Cont (fmap f c)

instance Functor (Consumer i) where
    fmap f (C c) = C $ fmap f . c

firstJust' :: Consumer (Maybe a) (Maybe a)
firstJust' = join <$> findI isJust

-- ghci> iter eg firstJust'
