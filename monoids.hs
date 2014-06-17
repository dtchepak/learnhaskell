import Control.Applicative

class Monoid a where
    (<>) :: a -> a -> a
    mzero :: a

instance Monoid b => Monoid (a -> b) where
    (<>) = liftA2 (<>)
    mzero = const mzero

newtype And = And Bool
    deriving (Show, Eq)

instance Monoid And where
    And a <> And b = And $ a && b
    mzero = And True
