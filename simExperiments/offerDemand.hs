import Prelude hiding( id, (.))
import Control.Category

type Time = Int
type Event a = Maybe (Time, a)

data Offer  = Offer Int
data Accept = Accept Int

newtype ProcessM m a b = ProcessM {runProcessM :: a -> m (b, ProcessM m a b) }

instance Monad m => Category (ProcessM m) where
  id    = ProcessM $ \x -> return (x, id)
  g . f = ProcessM $ \x -> do
    (y, f') <- runProcessM f x
    (z, g') <- runProcessM g y
    return (z, g' . f')


