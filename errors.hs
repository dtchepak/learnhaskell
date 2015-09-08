import Control.Applicative
import Control.Monad

(?|) :: Maybe a -> e -> Either e a
a ?| e = maybe (Left e) Right a

sample :: (Int, Int) -> Either String (Int,Int)
sample (a,b) = do
    guard (even a) ?| "first arg must be even"
    guard (b > 10) ?| "second arg must be >10"
    return (a,b)

