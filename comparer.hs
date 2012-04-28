data Car = Car { make :: String, model :: String } deriving (Eq,Show)

instance Ord Car where
    compare (Car { make = aMake, model = aModel}) (Car { make = bMake, model = bModel}) = 
        if makeCompare == EQ then aModel `compare` bModel else makeCompare
        where makeCompare = aMake `compare` bMake

comp :: (Ord a) => [b -> a] -> b -> b -> Ordering
comp [] _ _ = EQ
comp (x:xs) a b = if (propCompare == EQ) then comp xs a b else propCompare
    where aProp = x a
          bProp = x b
          propCompare = aProp `compare` bProp

