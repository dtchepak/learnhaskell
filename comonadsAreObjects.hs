-- http://www.haskellforall.com/2013/02/you-could-have-invented-comonads.html
type Option = String
data Config = MakeConfig [Option] deriving (Show)

configBuilder :: [Option] -> Config
configBuilder = MakeConfig

defaultConfig :: [Option] -> Config
defaultConfig options = MakeConfig (["-Wall"] ++ options)

profile :: ([Option] -> Config) -> Config
profile builder = builder ["-prof", "-auto-all"]

goFaster :: ([Option] -> Config) -> Config
goFaster builder = builder ["-O2"]
