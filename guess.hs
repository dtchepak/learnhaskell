import System.Random
import Data.Maybe
import Control.Monad(forever)


main = forever $ do
    gen <- newStdGen
    play . fst . randomR (1,10) $ gen

play :: Int -> IO ()
play n = do
    putStrLn ""
    putStrLn "What number am I thinking of between 1 and 10?"
    guess <- getLine
    let result = checkGuess (readMaybe guess) n
    putStrLn (show result)
    if result == Correct 
        then return ()
        else play n

data Guess = NotANumber | TooLow | TooHigh | Correct deriving (Eq)
instance Show Guess where
    show TooLow = "Too low"
    show TooHigh = "Too high"
    show Correct = "Correct!"
    show NotANumber = "You need to guess a number between 1 and 10"

checkGuess :: Maybe Int -> Int -> Guess
checkGuess Nothing _ = NotANumber
checkGuess (Just guess) actual
    | guess < actual = TooLow
    | guess > actual = TooHigh
    | otherwise      = Correct

-- readMaybe defn from http://stackoverflow.com/a/8067014/906
readMaybe :: Read a => String -> Maybe a
readMaybe s = case reads s of
              [(x, "")] -> Just x
              _ -> Nothing


