{-# LANGUAGE RankNTypes #-}
-- http://blog.tmorris.net/posts/refactoring-puzzle/
module RefactoringPuzzle where

newtype IntRdr a =
  IntRdr {
    readIntRdr :: Int -> a
  }

mapIntRdr ::
  IntRdr a
  -> (a -> b)
  -> IntRdr b
mapIntRdr (IntRdr g) f =
  IntRdr (f . g)

bindIntRdr ::
  IntRdr a
  -> (a -> IntRdr b)
  -> IntRdr b
bindIntRdr (IntRdr g) f =
  IntRdr (\n -> readIntRdr (f (g n)) n)

applyIntRdr ::
  a
  -> IntRdr a
applyIntRdr =
  IntRdr . const

type Option = Maybe

mapOption ::
  Option a
  -> (a -> b)
  -> Option b
mapOption Nothing _ =
  Nothing
mapOption (Just a) f =
  Just (f a)

bindOption ::
  Option a
  -> (a -> Option b)
  -> Option b
bindOption Nothing _ =
  Nothing
bindOption (Just a) f =
  f a

applyOption ::
  a
  -> Option a
applyOption a =
  Just a

-- Return all the Some values, or None if not all are Some.
runOptions :: [Option a] -> Option [a]
--runOptions = foldr (\a b -> bindOption a (\aa -> mapOption b (aa:))) (applyOption [])
runOptions = seqThingoe optThing

-- Apply an Int to a list of int readers and return the list of return values.
runIntRdrs :: [IntRdr a] -> IntRdr [a]
--runIntRdrs = foldr (\a b -> bindIntRdr a (\aa -> mapIntRdr b (aa:))) (applyIntRdr [])
runIntRdrs = seqThingoe intRdrThing

data Thingoe f = Thingoe { 
    bind :: forall a b. f a -> (a -> f b) -> f b,
    ret  :: forall a.   a -> f a
}

map' :: forall f a b. Thingoe f -> (a -> b) -> f a -> f b
map' t f fa = bind t fa (ret t . f)

seqThingoe :: Thingoe f -> [f a] -> f [a]
seqThingoe t = foldr (\a b -> bind t a (\aa -> map' t (aa:) b)) (ret t [])

intRdrThing :: Thingoe IntRdr
intRdrThing = Thingoe bindIntRdr applyIntRdr

optThing :: Thingoe Option
optThing = Thingoe bindOption applyOption

-- Code Duplication

-- ***      ******      *******      ****
-- runOptions :: [Option a] -> Option [a]
-- runIntRdrs :: [IntRdr a] -> IntRdr [a]

-- ***      ***********************      **************      ************           ****
-- runOptions = foldr (\a b -> bindOption a (\aa -> mapOption b (aa:))) (applyOption [])
-- runIntRdrs = foldr (\a b -> bindIntRdr a (\aa -> mapIntRdr b (aa:))) (applyIntRdr [])

