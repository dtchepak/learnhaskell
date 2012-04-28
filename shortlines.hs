main = do
    --contents <- getContents
    --putStr (shortLinesOnly contents)
    interact shortLinesOnly

shortLinesOnly :: String -> String
shortLinesOnly = unlines . filter ((<10) . length) . lines
