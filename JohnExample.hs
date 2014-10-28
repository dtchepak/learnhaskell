import Control.Monad.Trans.Either -- cabal install either
import Control.Applicative
import Safe (atMay) -- cabal install safe

type CSV = [[String]]
type ParseError = String

parseCSVFromFile :: FilePath -> IO (Either ParseError CSV)
parseCSVFromFile _ = pure $ Right [["1","2","3"],["4","5","6"]]

parseInt :: String -> Either ParseError Int
parseInt s =
    let doParse [(i, [])] = pure i 
        doParse _ = Left ("could not read int: " ++ s)
    in doParse (reads s)

data F = F Int Int Int
    deriving (Show,Eq)

parseF :: [String] -> Either ParseError F
parseF (a:b:c:_) = F <$> parseInt a <*> parseInt b <*> parseInt c 
parseF _ = Left "wrong number of args"

main :: IO ()
main =
    let readCsv = parseCSVFromFile "thefile.csv"
        parseFs = fmap (>>= mapM parseF)
    in parseFs readCsv >>= either print print

---------------------------------------
-- Lookup argument, then parse.
--
-- Reduces pattern matching on args, instead moving it to an explicit
-- lookup. (indexing in to linked list is pretty yuck, would want
-- a different type.)
yukkyParseF :: [String] -> Either ParseError F
yukkyParseF args =
    let tryGet i = maybe
                     (Left $ "could not find arg at " ++ show i)
                     Right
                     . (flip atMay i)
        makeF = F <$> (parseInt =<< tryGet 0 args)
                  <*> (parseInt =<< tryGet 1 args)
                  <*> (parseInt =<< tryGet 2 args)
    in makeF
  -- I tried using an EitherT stacked with Reader so we could use
  -- F <$> (parseInt <=< tryGet 0) <*> ...
  -- (rather than passing `args` through everywhere)
  -- but I got a bit lost lifting things into the stack.
  -- If you're interested in that approach let me know and I'll
  -- spend a bit more time on it.

main2 :: IO ()
main2 =
    let readCsv = parseCSVFromFile "thefile.csv"
        parseFs = fmap (>>= mapM yukkyParseF)
    in parseFs readCsv >>= either print print


---------------------------------------
-- With EitherT transformer stack
-- Simpifies working with the IO (Either ParseError a) types
--
readFs :: FilePath -> EitherT ParseError IO [F]
readFs src =
  EitherT (parseCSVFromFile src)
    >>= hoistEither . mapM parseF

main3 :: IO ()
main3 =
  eitherT print print (readFs "thefile.csv")

