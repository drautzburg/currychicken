import Data.Function


data Event t v = Evt {
    getE :: t -> (t,v)
    }

exevt1 = Evt $ \t -> (t,1)
exevt2 = Evt $ \t -> (t+1,2)

data Events t v = Evts {
    getEs :: t -> [(t,v)]
    }


liftE f  = Evt . fmap f . getE
liftEs f = Evts . fmap f . getEs


onFst :: (a->b) -> (a,x) -> (b,x)
onFst f (x,y) = (f x,y)

onSnd :: (a->b) -> (x,a) -> (x,b)
onSnd f (x,y) = (x,f y)


instance Functor (Event t) where
    fmap f = Evt . fmap (onSnd f) . getE 

instance (Ord t) => Applicative (Event t) where
    pure a = Evt $ \t -> (t,a)
    ef <*> ev = Evt $ \tx -> let (tf, f) = getE ef tx
                                 (tv, v) = getE ev tx
                             in (max tf tv, f v)


instance Functor (Events t) where
    fmap = liftEs . fmap . fmap
    


instance (Ord t) => Applicative (Events t) where
    pure v    = Evts $ \tx -> [(tx,v)]
    af <*> av = Evts $ \tx -> let fs = getEs af tx
                                  vs = getEs av tx
                              in undefined

-- Can't do that, don't know how to pair functions and values, as
-- there is no value before the first change
