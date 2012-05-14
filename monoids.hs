
newtype Pair b a = Pair { getPair :: (a,b) }

replace :: Pair a a -> a -> Pair a a
replace (Pair { getPair = (x,y) }) z = Pair ((z,z))

instance Functor ((,) r) where
    fmap f (a,b) = (a,f b)

data CoolBool = CoolBool { getCoolBool :: Bool }
helloMe :: CoolBool -> String
helloMe (CoolBool _) = "hello"

newtype CoolBool2 = CoolBool2 { getCoolBool2 :: Bool }
helloMe2 :: CoolBool2 -> String
helloMe2 (CoolBool2 _) = "hello"
