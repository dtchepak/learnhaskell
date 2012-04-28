
chop :: Ord a => a -> [a] -> Int
chop _ [] = -1
chop find sortedList = case compare find midpointValue of
                        EQ -> midpoint
                        LT -> chop find left
                        GT -> midpoint + (chop find right)
    where midpoint = (length sortedList) `div` 2
          midpointValue = sortedList !! midpoint
          left = take (midpoint-1) sortedList
          right = drop midpoint sortedList
