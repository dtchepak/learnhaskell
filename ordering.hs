-- http://www.markhneedham.com/blog/2012/05/23/haskell-using-monoids-when-sorting-by-multiple-parameters/
--

import Data.Ord
import Data.List
import Data.Monoid

data Row = Row { shortListed :: Bool, cost :: Float, distance1 :: Int, distance2 :: Int } deriving (Show, Eq)

 
compareRow :: Row -> Row -> Ordering
compareRow x y = if comparing (not . shortListed) x y == EQ then 
                    if comparing cost x y == EQ then
                      if comparing distance1 x y == EQ then
                        comparing distance2 x y
                      else
                        comparing distance1 x y
                    else
                      comparing cost x y
                  else 
                    comparing (not . shortListed) x y

compareRow' :: Row -> Row -> Ordering
compareRow' x y = mconcat [comparing (not . shortListed) x y,  comparing cost x y, comparing distance1 x y, comparing distance2 x y]
 
