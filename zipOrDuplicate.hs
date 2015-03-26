import Control.Applicative
import Data.Maybe

{- zip two lists. If one is over, duplicate elements of the remaining list.

   Examples:

   >>> zipOrDuplicate [1,2,3] [0]
   [(1,0),(2,2),(3,3)]

   >>> zipOrDuplicate [0] [1,2,3]
   [(0,1),(2,2),(3,3)]

   >>> zipOrDuplicate "hello" "world"
   [('h','w'),('e','o'),('l','r'),('l','l'),('o','d')]
-}
zipOrDuplicate [] [] = []
zipOrDuplicate xs [] =
    let dupe a = (a,a)
    in map dupe xs
zipOrDuplicate [] ys = zipOrDuplicate ys []
zipOrDuplicate (x:xs) (y:ys) =
    (x,y):zipOrDuplicate xs ys
--
-- based on http://codepad.org/n1fF9nN2
{-
zipOrDuplicate2 xs ys =
    let pad = (++ repeat Nothing) . map Just
        pair x y = ((,) <$> x <*> y) <|> ((\z -> (z,z)) <$> (x <|> y))
    in catMaybes $ takeWhile isJust $ zipWith pair (pad xs) (pad ys)
-}
