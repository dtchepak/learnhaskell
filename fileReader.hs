import System.Environment
import System.IO
import System.IO.Error

main = showFileContents `catch` handle

showFileContents :: IO()
showFileContents = do
    (fileName:_) <- getArgs
    contents <- readFile fileName
    putStr contents

handle :: IOError -> IO()
handle _ = do
    putStrLn "Oh noes!!!1!!"

