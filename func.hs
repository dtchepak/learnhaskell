{-# LANGUAGE ExistentialQuantification #-}
data Func a = forall x. Func (x -> a)

runFunc :: forall x a. Func a -> x -> a
runFunc (Func f) x = f x

instance (Functor Func) where
    fmap f (Func func) = Func (f . func)
