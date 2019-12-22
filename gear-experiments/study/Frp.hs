{-|
Module      : W
Description : Poor man's FRP implementation
Copyright   : (c) Martin Drautzburg 2018
License     : GPL-3
Maintainer  : Martin.Drautzburg@web.de
Stability   : experimental
Portability : POSIX

This module implements the cors FRP functions. It is intended to
simplify working with Events and not for building reactive software.

-}      


{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE BangPatterns #-}
module Frp where
import Data.Function
import Data.Tuple
import qualified Data.List as L

-- * Events and Behaviors

-- | A possibly infinite list of (Time, Value) pairs
newtype Event t a = Evt {
------------------------------------------------------------
    unE :: [(t,a)]
    } deriving (Eq, Show, Functor)


               
-- lift a function between lists to Event
onE :: ([(t1, a1)] -> [(t2, a2)]) -> Event t1 a1 -> Event t2 a2
onE f = Evt . f . unE


nullE :: Event t a -> Bool
nullE = null . unE
        
-- first time and value. Fails when empty
t0e = fst . head . unE             
v0e = snd . head . unE

-- lift a function between (t,v) to a function between lists thereof
-- is simply map

swapE :: Event a b -> Event b a
swapE = (onE . map) swap

------------------------------------------------------------
data Behavior t a = Beh {
------------------------------------------------------------
    val0   :: a,
    chgs :: Event t a
    } deriving (Eq, Show, Functor)

instance (Ord t) => Applicative (Behavior t) where
    pure a = Beh a (Evt [])
    -- (<*>) :: Applicative f => f (a -> b) -> f a -> f b
    f <*> x = Beh (val0 f $ val0 x) (Evt (weave f x))
          where
              weave :: (Ord t) => Behavior t (a->b) -> Behavior t a -> [(t,b)]
              weave g y
                | isConst g && isConst y = []
                | g `preceeds` y         = (t0b g, v0b g (val0 y)) : weave (shift g) y
                | otherwise              = (t0b y, val0 g (v0b y)) : weave g (shift y)

-- xxx use compare instead of preceeds. Handle simultaneous evts-

-- time and value of first change, fails when const
t0b = t0e . chgs      
v0b = v0e . chgs

exb1 = beh 0 [(t,t) | t <- [10,20..50]]
exb2 = beh 0 [(t,t) | t <- [5,10..25]]
       
beh a evts = Beh a (Evt evts)

bconst a = beh a []
isConst = nullE. chgs

upTo :: Ord t => t -> Behavior t a -> Event t a
upTo t = Evt . takeWhile ((<=t) . fst) . unE . chgs

after :: Ord t => t -> Behavior t a -> Event t a
after t = Evt . dropWhile ((<=t) . fst) . unE . chgs

preceeds a b
      | isConst a && isConst b = False
      | isConst a              = False
      | isConst b              = True
      | otherwise              = t0 a < t0 b
      where t0 = fst . head . unE . chgs

shift :: Behavior t a -> Behavior t a
shift beh = case beh of
    Beh a (Evt [])     -> beh
    Beh a (Evt (!e:es)) -> Beh (snd e) (Evt es)


at :: Ord t => t -> Behavior t a -> a
at t beh = case upTo t beh of
                  Evt [] -> val0 beh
                  Evt xs -> snd $ last xs
beh !@ t = at t beh
       
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
    Evt (f:fs) -> let (t',a') = (fst f, snd f a) 
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
