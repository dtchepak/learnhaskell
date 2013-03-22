import Control.Applicative ((<|>))
import Control.Monad
import Data.Function
import Data.Maybe

empty = Just []
full = Just [1,2,3]
none = Nothing


data TestRun a = TestRun (a,a)
    deriving (Show)

combinations :: [TestRun (Maybe [Int])]
combinations =
    [ TestRun (empty, empty)
    , TestRun (empty, full)
    , TestRun (full, empty)
    , TestRun (full, full)
    , TestRun (none, empty)
    , TestRun (empty, none)
    , TestRun (none, full)
    , TestRun (full, none)
    ]

runTest :: TestRun (Maybe [Int]) -> String
runTest (TestRun (a,b)) = show (f a b)

f :: Maybe [a] -> Maybe [a] -> [a]
f = (++) `on` fromMaybe []

showTestResult test = show test ++ "\n    " ++ (runTest test)

main =
    let results = map (putStrLn . showTestResult) combinations
    in sequence_ results
