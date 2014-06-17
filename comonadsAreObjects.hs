-- http://www.haskellforall.com/2013/02/you-could-have-invented-comonads.html
type Option = String
data Config = MakeConfig [Option] deriving (Show)

configBuilder :: [Option] -> Config
configBuilder = MakeConfig

defaultConfig :: [Option] -> Config
defaultConfig options = configBuilder (["-Wall"] ++ options)

profile :: ([Option] -> Config) -> Config
profile builder = builder ["-prof", "-auto-all"]

goFaster :: ([Option] -> Config) -> Config
goFaster builder = builder ["-O2"]

-- how to compose profile and goFaster ??
type ConfigBuilder = [Option] -> Config

profile' :: ConfigBuilder -> ConfigBuilder
profile' builder = \options -> builder (["-prof", "-auto-all"] ++ options)

goFaster' :: ConfigBuilder -> ConfigBuilder
goFaster' builder = \options -> builder (["-O2"] ++ options)

extract :: ([Option] -> Config) -> Config
extract = ($ [])

(#) :: a -> (a -> b) -> b
x # f = f x
infixl 0 #

-- setter :: ConfigBuilder -> Config
-- extend builder :: ConfigBuilder -> ConfigBuilder
-- therefore:
--  extend :: (ConfigBuilder -> Config) -> ConfigBuilder -> ConfigBuilder
--
-- cobind :: Comonad c => (c a -> b) -> c a -> c b

class Comonad c where
    counit :: c a -> a
    cobind :: (c a -> b) -> c a -> c b

data Foo = Foo
data Builder a = B ([Option] -> a)
instance Comonad Builder where
    -- counit :: Builder a -> a
    counit (B b) =  b []
    -- w :: Builder a -> b
    cobind w (B ba) = B $ \optsB ->
                        w (\optsA -> ba (optsA ++ optsB))


