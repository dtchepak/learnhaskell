{- LANGUAGE OverloadedStrings -}
import Control.Applicative ((<*))
import Data.Functor ((<$))
import Data.Text (Text)
import Text.Parsec
import Text.Parsec.Text (GenParser)
type Parser = GenParser ()

defaultDelim :: Parser Char
defaultDelim = newline <|> char ','    -- <|> is "or". default delimiter is newline or comma

customDelim :: Parser (Parser Char)
customDelim  = do
    count 2 (char '/')
    delim <- anyChar
    newline
    return $ char delim

stringCalcP :: Parser [Int]
stringCalcP = do
    delim <- customDelim <|> return defaultDelim    -- Look for custom delim, or use default delim parser.
    empty <|> positive `sepBy` delim                -- Empty or delimited list of positive ints

add :: Text -> Maybe Int
add = parseAll $ fmap sum stringCalcP


--------------------
-- Helpers
-- Not sure if these are already in Parsec somewhere.

empty :: Parser [a]
empty = [] <$ eof

positive :: Parser Int
positive = read `fmap` many1 digit

parseAll :: Parser a -> Text -> Maybe a
parseAll p s = 
    let parseToEnd = p <* eof    -- matches p then eof, but only returns value from p
        parsed = parse parseToEnd [] s
    in either (const Nothing) Just parsed

