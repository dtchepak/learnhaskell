import Control.Monad ((>>=))
import Control.Applicative ((<$>))

ap :: (Monad m, Functor m) => m (a -> b) -> m a -> m b
--f `ap` fa = f >>= \f' -> fa >>= return . f'
--f `ap` fa = f >>= (fa >>= (return.))
f `ap` fa = f >>= (<$> fa)



