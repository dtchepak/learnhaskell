
import Data.Char

toUppercase a 
    | charCode >= 97 && charCode <= 122   = chr(charCode - 32)
    | otherwise                           = a
    where charCode = ord a

capitalize = map toUppercase



