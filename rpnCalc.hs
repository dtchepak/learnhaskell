
solveRPN :: String -> Double
solveRPN = (head . foldl process [] . words)
    where process (x:y:ys) "+" = (x+y):ys
          process (x:y:ys) "-" = (x-y):ys
          process (x:y:ys) "*" = (x*y):ys
          process (x:y:ys) "/" = (x/y):ys
          process stack item   = read item : stack

-- f (g s)
-- (f . g) s
-- oo
