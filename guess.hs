import System.Random
import Data.Maybe
import Control.Monad(forever)


main = forever $ do
    gen <- newStdGen
    play 1 . fst . randomR (1,10) $ gen

play :: Int -> Int -> IO ()
play guessNumber n = do
    putStrLn ""
    putStrLn "What number am I thinking of between 1 and 10?"
    result <- fmap (flip checkGuess n . readMaybe) getLine
    putStrLn (show result)
    if result == Correct 
        then 
            let guessString = if guessNumber == 1 then "guess" else "guesses" 
            in
                putStrLn $ "It took you " ++ show guessNumber ++ " " ++ guessString ++ "."
        else play (guessNumber+1) n

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


