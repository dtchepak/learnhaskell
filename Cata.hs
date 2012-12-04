import Control.Monad
import Test.QuickCheck
import Test.QuickCheck.Arbitrary

data Option a = Empty | Full a
    deriving (Show, Eq)

option :: b -> (a -> b) -> Option a -> b
option valIfEmpty _ Empty = valIfEmpty
option _ f (Full a) = f a

instance Arbitrary a => Arbitrary (Option a) where
  arbitrary = frequency [(1, return Empty), (3, liftM Full arbitrary)]
  shrink (Full x) = Empty : [ Full x' | x' <- shrink x ]
  shrink _        = []

data Foo = Bar Int | Zap String [Int] | Giraffe String
    deriving (Show, Eq)

foo :: (Int -> b) -> (String -> [Int] -> b) -> (String -> b) -> Foo -> b
foo f _ _ (Bar x) = f x
foo _ f _ (Zap x y) = f x y
foo _ _ f (Giraffe x) = f x


instance Arbitrary Foo where
  arbitrary = frequency [ (1, liftM Bar arbitrary)
                        , (1, liftM2 Zap arbitrary arbitrary)
                        , (1, liftM Giraffe arbitrary)]





