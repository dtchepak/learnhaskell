import Control.Monad
import Control.Monad.State
import System.Random

data GuessState = GuessState { actual :: Int, attempts :: Int}
    deriving Eq
data GuessResult = Low | High | Equal
    deriving Eq

instance Show GuessResult where
    show Low = "Too low"
    show High = "Too high"
    show Equal = "Got it!"

result :: Int -> Int -> GuessResult
result guess actual
    | guess < actual = Low
    | guess > actual = High
    | otherwise      = Equal

main = do
    n <- randomRIO (1 :: Int, 10)
    gameLoop (GuessState n 0)

game :: StateT GuessState IO GuessResult
game = undefined

gameLoop :: GuessState -> IO ()
gameLoop (GuessState actual attempts) = do
    putStrLn "Guess the number between 1 and 10: "
    guess <- read `fmap` getLine
    let attempts' = attempts+1
    let thisResult = result guess actual
    putStrLn $ show thisResult
    case thisResult of
        Equal -> putStrLn $ "You guessed it in " ++ show attempts'
        _     -> gameLoop (GuessState actual attempts')

--game :: Int -> State GuessState (IO GuessResult)
--game guess = do
--    s <- get
--    put s { a

--gameLoop :: Int -> IO ()
--gameLoop n = do
--    putStrLn "Guess the number between 1 and 10: "
--    guess <- getLine
--    
--
--main = do
--    n <- randomRIO (1 :: Int,10)
--    gameLoop n
--

