{-|
Module      : W
Description : Poor man's FRP implementation
Copyright   : (c) Martin Drautzburg 2018
License     : GPL-3
Maintainer  : Martin.Drautzburg@web.de
Stability   : experimental
Portability : POSIX

This module implements the core FRP functions. 'Behavior' is
essentially a function from Time, with no way of finding out when it
changes.


This module is only intended to simplify working with Events within
'Gear' and not for building actual reactive software.

-}      


{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE PartialTypeSignatures #-}
module Gear.FrpSmooth where
import Data.Function
import Data.Tuple
import Data.Maybe
import Control.Applicative
import qualified Data.List as L
import qualified Data.EventList.Absolute.TimeBody as Tb
import qualified Control.Monad as M
import Test.QuickCheck -- (Arbitrary(arbitrary, shrink))

-- * Core functions

-- | Successively apply function-events to a starting value                      
accumE  :: a -> Events t (a -> a) -> Events t a
accumE a evts = case viewL evts of
  Nothing -> emptyE
  Just ((t,f),fs) -> let a' = f a
                     in Tb.cons t a' (accumE a' fs)

-- | Create a 'Behavior' from a default value and change 'Events'
stepper :: (Ord t) => a -> Events t a -> Behavior t a
stepper a evts = Beh $ \t -> (f . takeWhile ((<= t). fst) . toPairList) evts
  where f [] = a
        f es = snd $ last es


-- | Filter events that satisfy a predicate
filterE  :: Num t => (a -> Bool) -> Events t a -> Events t a
filterE = Tb.filter 

-- | Apply a 'Behavior' whose values are functions to 'Events'
--
-- >>> (toPairList . takeWhileT (<12) . applyE exBehf) exEvti
-- [(5,10),(6,12),(7,14),(8,16),(9,18),(10,20),(11,22)]

applyE :: (Ord t,Num t) => Behavior t (a -> b) -> Events t a -> Events t b
applyE beh  = fromPairList . fmap f . toPairList 
  where
    f (t, v) = (t, at beh t v)

-- * Events

type Events = Tb.T 

-- ** Construction

fromPairList :: [(t,a)] -> Events t a
fromPairList = Tb.fromPairList

toPairList :: Events t a -> [(t,a)]
toPairList = Tb.toPairList

emptyE = Tb.empty

fromTimes :: (t->a) -> [t] -> Events t a
fromTimes f ts = fromPairList [(t, f t) | t <- ts]

-- ** Deconstruction

viewL :: Events t a -> Maybe ((t,a), Events t a)
viewL = Tb.viewL


-- ** Examples

exEvts :: Events Int String
exEvts = fromTimes (("Evt" ++) . show)[1..10]

exEvti :: Events Int Int
exEvti = fromTimes id [5..]

exEvtf :: Events Int (Int->Int)
exEvtf = fromTimes (+) [0..] 


-- * Behavior

------------------------------------------------------------
-- | Behavior is a function from time

data Behavior t a = Beh {
    at :: t -> a
    } deriving (Functor)
------------------------------------------------------------

instance Ord t => Applicative (Behavior t) where
  pure = Beh . const
  bf <*> ba = Beh $ \t -> let f = at bf t
                              a = at ba t
                          in f a
-- ** Filtering 

-- | By time
takeWhileT :: Ord t => (t->Bool) -> Events t a -> Events t a
takeWhileT pt = fromPairList . takeWhile (pt . fst) . toPairList


-- ** Example

exBehs :: Behavior Int String
exBehs = stepper "strings" exEvts

exBehi :: Behavior Int Int
exBehi = stepper  0 exEvti

exBehf :: Behavior Int (Int->Int)
exBehf = stepper (*3) exEvtf
