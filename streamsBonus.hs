-- http://lpaste.net/107655
module Data.Stream where
infixr 5 :<

data Stream a = a :< Stream a

instance Show a => Show (Stream a) where
    show s = '(' : (go 5 s) ++ ")"
      where go 0 _ = "..."
            go n (h:<t) = show h ++ " :< " ++ go (n-1) t

instance Functor Stream where
    fmap f (h:<t) = f h :< fmap f t

class Functor w => Comonad w where
    extract :: w a -> a
    extend :: (w a -> b) -> w a -> w b
    extend f = fmap f . duplicate
    duplicate :: w a -> w (w a)
    duplicate = extend id

infixl 1 =>>
(=>>) :: Comonad w => w a -> (w a -> b) -> w b
(=>>) = flip extend

instance Comonad Stream where
    extract (h:<_) = h
    extend f w@(_:<t) = f w :< (extend f t)
    --duplicate w@(_:<t) = w :< duplicate t


ones :: Stream Integer
ones = 1 :< ones

nats :: Stream Integer
nats = 0 :< (nats =>> succ . extract)

fibs :: Stream Integer
fibs = 0 :< 1 :< (fibs =>> \(a:<b:<_) -> a+b)

takeS :: Int -> Stream a -> [a]
takeS 0 _ = []
takeS n (h:<t) = h : takeS (n-1) t

dropS :: Int -> Stream a -> Stream a
dropS n s@(_:<t)
    | n <= 0    = s
    | otherwise = dropS (n-1) t

one :: Stream Double
one = 0.5 :< (one =>> (0.5*) . extract)

