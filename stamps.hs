
data Stamp = Red | Yellow | Green
    deriving (Eq, Show)

-- a did not rule out b & c having the same colour, so they return false
aObs (Red, Red) = False
aObs (Yellow, Yellow) = False
aObs _ = True

-- b did not rule of a & c having the same colour, so they return false
bObs (Red, Red) = False
bObs (Yellow, Yellow) = False
-- b also knows that a didn't observe b & c having the same colour, else it
-- would have returned false. So only valid cases are when a doesn't rule
-- out b and c having the same colour.
bObs (_, c) = aObs (c, c)

-- any combination that b doesn't rule out based on a and c are valid
cObs c (a,_) = bObs (a,c)

-- all possible combinations
combs = 
    let 
        stamps = [Red,Yellow,Green]
        all_combs = [(a,b,c) | a<-stamps, b<-stamps, c<-stamps]
    in filter (\s -> s /= (Red,Red,Red) && s /= (Yellow,Yellow,Yellow)) all_combs

-- all valid combination
validCombs = filter (\(a,b,c) -> cObs c (a,b)) combs


