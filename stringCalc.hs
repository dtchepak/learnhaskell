import Data.Char
import Data.Maybe

newline      = is '\n'
comma        = is ','
defaultDelim = newline ||| comma
customDelim  = do 
    is '/'
    is '/'
    delim <- char
    newline
    return $ is delim

stringCalcP :: Parser [Int]
stringCalcP = do
    delim <- customDelim ||| return defaultDelim
    empty ||| sepBy delim positive

stringCalc :: String -> Maybe Int
stringCalc = 
    fmap sum . parseAll stringCalcP

----------------------
-- Parser stuff
-- Should use one of the parsing libraries instead.
----------------------

data Parser a = P { parse :: String -> Maybe (String, a) }

instance Functor Parser where
    fmap f (P p) = P $ \i -> (fmap . fmap) f (p i)

instance Monad Parser where
    return = constP
    (P p) >>= f = P $ \i -> p i >>= \(i',a) -> parse (f a) i'

constP :: a -> Parser a
constP a = P $ \i -> Just (i, a)

failP :: Parser a
failP = P $ const Nothing

(|||) :: Parser a -> Parser a -> Parser a
(P p) ||| (P q) = P $ \i -> case p i of
                                Nothing -> q i
                                result  -> result

char :: Parser Char
char = P $ \i -> case i of
                    []     -> Nothing
                    (c:cs) -> Just (cs, c)

match :: (Char -> Bool) -> Parser Char
match pred = char >>= \a -> if pred a then constP a else failP

is :: Char -> Parser Char
is = match . (==)

atLeast1 :: Parser a -> Parser [a]
atLeast1 p = p >>= \a -> fmap (a:) (many p)

many :: Parser a -> Parser [a]
many p = atLeast1 p ||| constP []

positive:: Parser Int
positive = fmap read (atLeast1 (match isDigit))

empty :: Parser [a]
empty = P $ \i -> case i of
                    [] -> Just (i, [])
                    _  -> Nothing

sepBy :: Parser s -> Parser a -> Parser [a]
sepBy s p = 
    let sepAndOne = s >> p
    in do
        first <- p
        rest <- many sepAndOne
        return $ first:rest

sepByChar :: Char -> Parser a -> Parser [a]
sepByChar = sepBy . is

parseAll :: Parser a -> String -> Maybe a
parseAll p s = case parse p s of
                Just ([], a) -> Just a
                _               -> Nothing


