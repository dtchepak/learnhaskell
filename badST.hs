-- source: http://stackoverflow.com/a/11679271/906

{-# LANGUAGE GeneralizedNewtypeDeriving #-}

import Control.Monad
import Data.IORef
import System.IO.Unsafe
newtype ST s a = ST { unST :: IO a } deriving Monad
newtype STRef s a = STRef { unSTRef :: IORef a }
alternativeRunST :: ST s a -> a
alternativeRunST = unsafePerformIO . unST
newSTRef :: a -> ST s (STRef s a)
newSTRef = ST . liftM STRef . newIORef
readSTRef :: STRef s a -> ST s a
readSTRef = ST . readIORef . unSTRef
writeSTRef :: STRef s a -> a -> ST s ()
writeSTRef ref = ST . writeIORef (unSTRef ref)

leakyVar :: STRef s Int
leakyVar = alternativeRunST (newSTRef 0)

evilFunction :: Int -> Int
evilFunction x =
  alternativeRunST $ do
    val <- readSTRef leakyVar
    writeSTRef leakyVar (val+1)
    return (val + x)
