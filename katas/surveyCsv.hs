import Data.Char
import Data.List
import Data.Maybe
import Data.Monoid
import Data.Traversable (traverse)

data ProgrammingLanguage 
    = CSharp
    | FSharp
    | Haskell
    | Ruby
    | JavaScript
    deriving (Eq, Show)

data SurveyResponse = 
    Resp { site :: String
         , results :: [(ProgrammingLanguage, Int)]
         }

type Col = (String, SurveyResponse -> String)

reportColumns :: [Col]
reportColumns =
    let lower = fmap toLower
        lookupCol :: ProgrammingLanguage -> Col
        lookupCol l = ( lower (show l)
                      , show . fromMaybe 0 . lookup l . results)
    in
        [ ("site", site)
        , lookupCol CSharp
        , lookupCol FSharp
        , lookupCol Haskell
        , lookupCol Ruby
        , lookupCol JavaScript
        ]

surveyToCsv :: [SurveyResponse] -> String
surveyToCsv =
    let joinOn   = intercalate
        toCsv    = joinOn ","
        headers  = toCsv $ fmap fst reportColumns
        values   = toCsv . traverse snd reportColumns
    in
        joinOn "\n" . (headers:) . fmap values

sample = 
    [ Resp "site1" [(CSharp, 3), (FSharp, 1), (Haskell, 0)]
    , Resp "site2" [(CSharp, 3), (Ruby, 5)]
    , Resp "site3" [(Ruby, 7), (JavaScript, 4)]
    ]

main = putStrLn $ surveyToCsv sample
