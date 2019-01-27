{-|
Module      : W
Description : Poor man's FRP implementation
Copyright   : (c) Martin Drautzburg 2018
License     : GPL-3
Maintainer  : Martin.Drautzburg@web.de
Stability   : experimental
Portability : POSIX

This module implements the core FRP functions. 'Behavior' is
essentially a function from Time. Its value changes at discrete points
it time an it has a Show instance.


This module is only intended to simplify working with Events within
'Gear' and not for building actual reactive software.

-}      


{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Gear.FrpTb where
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
stepper :: a -> Events t a -> Behavior t a
stepper = Beh

-- | Filter events that satisfy a predicate
filterE  :: Num t => (a -> Bool) -> Events t a -> Events t a
filterE = Tb.filter 

-- | Apply a 'Behavior' whose values are functions to 'Events'
--
-- >>> (toPairList . takeWhileT (<12) . applyE exBehf) exEvti
-- [(5,10),(6,12),(7,14),(8,16),(9,18),(10,20),(11,22)]
applyE :: (Ord t,Num t) => Behavior t (a -> b) -> Events t a -> Events t b
applyE beh evts = let beh'  = fmap fmap beh -- operate on Maybes
                      bevts = Beh Nothing (fmap Just evts)
                  in (fmap fromJust . filterE isJust . chgs) (beh' <*> bevts)

-- * Events

-- type Events t b = Tb.T t b
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


-- ** Filtering 

-- | By time
takeWhileT :: Ord t => (t->Bool) -> Events t a -> Events t a
takeWhileT pt = fromPairList . takeWhile (pt . fst) . toPairList


-- ** Examples

exEvts :: Events Int String
exEvts = fromTimes (("Evt" ++) . show)[1..10]

exEvti :: Events Int Int
exEvti = fromTimes id [5..]

exEvtf :: Events Int (Int->Int)
exEvtf = fromTimes (+) [0..] 


-- * Behavior

------------------------------------------------------------
-- | Conceptually a Behavior is a function from time, but it is
-- defined as default value and a list of change-'Events'.
--
-- For a Behavior (@beh :: Behavior t v@) 'at' gives you a function
-- from time (@at beh :: t -> v@)

data Behavior t a = Beh {
    vdef :: a,
    chgs :: Events t a
    } deriving (Eq, Show, Functor)
------------------------------------------------------------

-- | Get the value of the Behavior for a given time t
at :: Ord t => Behavior t a -> t -> a
at beh t = case (takeWhile ((<=t) . fst) . toPairList . chgs) beh of
                  [] -> vdef beh
                  xs -> (snd . last) xs

-- ** Constant Behavior

isConstB :: Behavior t a -> Bool
isConstB = null. chgs

constB :: a -> Behavior t a
constB a = Beh a emptyE

lift2 :: Ord t => (a->b->c) -> (Behavior t a) -> (Behavior t b) -> (Events t c)
lift2 f ba bb 
  | isConstB ba = (fmap  . f . vdef) ba (chgs bb)
  | isConstB bb = (fmap  . flip f . vdef) bb (chgs ba)
  | otherwise = let ((ta,va),ea) = (fromJust . viewL . chgs) ba
                    ((tb,vb),eb) = (fromJust . viewL . chgs) bb
                in case compare ta tb of
                     EQ -> let vc = f va vb
                           in Tb.cons ta vc $ lift2 f (Beh va ea) (Beh vb eb) 
                     LT -> let vc = f va (vdef bb)
                           in Tb.cons ta vc $ lift2 f (Beh va ea) bb
                     GT -> let vc = f (vdef ba) vb
                           in Tb.cons tb vc $ lift2 f ba (Beh vb eb) 

instance Ord t => Applicative (Behavior t) where
  pure a = Beh a emptyE
  bf <*> ba = let vb = (vdef bf) (vdef ba)
              in Beh vb $ lift2 ($) bf ba


-- ** Example

exBehs :: Behavior Int String
exBehs = Beh "strings" exEvts

exBehi :: Behavior Int Int
exBehi = Beh 0 exEvti

exBehf :: Behavior Int (Int->Int)
exBehf = Beh (*3) exEvtf
