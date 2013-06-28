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

data Result a =
    Done a
    | Cont (Maybe a -> Result a)
instance Show a => Show (Result a) where
    show (Done a) = show a
    show (Cont _) = "Cont"

success :: Maybe a -> Result a
success = maybe (Cont success) Done

iter :: [IO (Maybe a)] -> (Maybe a -> Result a) -> IO (Maybe a)
iter [] _ = return Nothing
iter (x:xs) f = 
    let step (Done a) = return (Just a)
        step (Cont c) = iter xs c
    in x >>= step . f

