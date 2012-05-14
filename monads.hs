
instance Monad Maybe where
    return x = Just x
    Nothing (>>=) _ = Nothing
    (Just x) (>>=) f = f x
    fail _ = Nothing
