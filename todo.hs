import System.Environment
import System.Directory
import System.IO
import Data.List

dispatch :: String -> [String] -> IO ()
dispatch "add" = add
dispatch "view" = view
dispatch "remove" = remove
dispatch _ = usage


main = do
    (command:argList) <- getArgs
    dispatch command argList

add :: [String] -> IO ()
add (file:task:[]) = appendFile file (task ++ "\n")
add _ = usage []

view :: [String] -> IO ()
view [file] = do
    contents <- readFile file
    let tasks = lines contents
    let numberedTasks = zipWith (\n line -> show n ++ " - " ++ line) [0..] tasks
    mapM_ putStrLn numberedTasks
view _ = usage []

remove :: [String] -> IO ()
remove _ = putStrLn "Not implemented."

usage :: [String] -> IO ()
usage _ = do
    putStrLn "Usage: "
    putStrLn "todo add (filename) (task name)"
    putStrLn "todo view (filename)"
    putStrLn "todo remove (filename) (task index)"



