
{-# LANGUAGE BangPatterns #-}
import Data.Function
import qualified Data.List as L

onFst :: (x->y) -> (x,a) -> (y,a)
onFst f (x,a) = (f x, a)

onSnd :: (a->b) -> (x,a) -> (x,b)
onSnd f (x,a) = (x, f a)

------------------------------------------------------------
data Event t a = Evt {
------------------------------------------------------------
    unE :: [(t,a)]
    }

instance Functor (Event t) where
    fmap f = Evt . (fmap . fmap) f . unE


upTo :: Ord t => t -> Behavior t a -> Event t a
upTo t = Evt . takeWhile ((<=t) . fst) . unE . chgs

after :: Ord t => t -> Behavior t a -> Event t a
after t = Evt . dropWhile ((<=t) . fst) . unE . chgs

-- onE :: ([(t,a)] -> [(t,b)]) -> Event t a -> Event t b
onE f = Evt . f . unE

------------------------------------------------------------
data Behavior t a = Beh {
------------------------------------------------------------
    v0   :: a,
    chgs :: Event t a
    }

beh a evts = Beh a (Evt evts)
bconst a = beh a []
isConst = null . unE . chgs


shift :: Behavior t a -> Behavior t a
shift beh = case beh of
    Beh a (Evt [])     -> beh
    Beh a (Evt (!e:es)) -> Beh (snd e) (Evt es)


at :: Ord t => t -> Behavior t a -> a
at t beh = case upTo t beh of
                  Evt [] -> v0 beh
                  Evt xs -> snd $ last xs

at' beh t = at t beh

filter  :: (a -> Bool) -> Event t a -> Event t a
filter p = Evt . L.filter (p . snd) . unE

stepper :: a -> Event t a -> Behavior t a
stepper = Beh


apply :: Ord t => Behavior t (a -> b) -> Event t a -> Event t b
apply beh evts = Evt $ case evts of
    Evt []     -> []
    Evt (e:es) -> let f (t,a) = (t, at t beh a) 
                  in f e : map f es


accumE  :: a -> Event t (a -> a) -> Event t a
accumE a evts = Evt $ case evts of
    Evt []     -> []
    Evt (f:fs) -> let (t',a') = (fst f, snd f a') 
                      l f = unE . f . Evt
                  in (t',a') : l (accumE a') fs


--acc :: a -> Event t (a -> a) -> Event t a
--acc evts = Evt $ case evts of

-- xxx extend this from [a] to Event a
-- f :: a -> [a->a] -> [a]
f a = let g = \a' -> ($ a')
          l g = Evt . g . unE
      in scanl g a 

exbf = beh (+10) [(i, (+ i)) | i <- [0,10..500001]] :: Behavior Int (Int->Int)
exba = beh 0 [(t,t) | t <- [0,10..500001]] :: Behavior Int Int
