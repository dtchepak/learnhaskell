import Control.Applicative

-- NOT a functor, as it doens't obey ID and composition laws
data CMaybe a = CNothing | CJust Int a deriving (Show)
instance Functor CMaybe where
    fmap f CNothing = CNothing
    fmap f (CJust counter x) = CJust (counter+1) (f x)

data MyMaybe a = MyNothing | MyJust a deriving (Show)

instance Functor MyMaybe where
    fmap _ MyNothing = MyNothing
    fmap f (MyJust x) = MyJust (f x)
instance Applicative MyMaybe where
    pure a = MyJust a  -- or just `pure = MyJust`
    (<*>) MyNothing = \x -> MyNothing  -- or write as MyNothing <*> _ = ...
    (<*>) (MyJust x) = fmap x
    

nonApplicativeMaybe Nothing _ = Nothing
nonApplicativeMaybe (Just x) y = fmap x y
