{-# LANGUAGE RankNTypes #-}
-- https://speakerdeck.com/markhibberd/lens-from-the-ground-up

import Control.Monad.Identity
import Control.Applicative

type Lens' a b =
    forall f. Functor f => (b -> f b) -> a -> f a

set :: Lens' a b -> b -> a -> a
set l b a = runIdentity $
    l (const . Identity $ b) a

get :: Lens' a b -> a -> b
get l a = getConst $
    l Const a

data Person = Person { _name :: String }
    deriving (Show)

name :: Lens' Person String
name m a = (\b -> Person { _name = b }) <$> m (_name a)

{-
ghci> let dave = Person "Dave"
ghci> get name dave
"Dave"
ghci> set name "David" dave
Person {_name = "David"}
ghci> name (Identity . map toUpper) dave
Identity (Person {_name = "DAVE"})
ghci> runIdentity $ name (Identity . map toUpper) dave
Person {_name = "DAVE"}
-}

modify :: Lens' a b -> (b -> b) -> a -> a
modify l m = runIdentity . l (Identity . m)
