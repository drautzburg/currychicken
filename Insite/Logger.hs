{-# LANGUAGE BangPatterns #-}
{-|
Module      : Des
Description : Combinators for creating 'Logger's
Copyright   : (c) Martin Drautzburg, 2015
License     : GPL-3
Maintainer  : Martin.Drautzburg@web.de
Stability   : experimental
Portability : POSIX
-}

module Logger where
import Des
import Data.Monoid
import Data.List

newtype Condition a  = Cnd {checkCnd :: a -> (Bool, Condition a)}



tBefore:: Instant -> Condition (Timed evt, dom)
tBefore ta = let cnd ((t,_),_) = (t < ta, tBefore ta)
            in Cnd cnd
            

tAfterEvery :: Instant -> Interval -> Condition (Timed evt, dom)
tAfterEvery t0 dt = let cnd ((t,_),_) = (t >= t0, tAfterEvery (t+dt) dt)
                   in Cnd cnd


tAfter:: Instant -> Condition (Timed evt, dom)
tAfter t0 = tAfterEvery t0 0.0

tEvery:: Instant -> Condition (Timed evt, dom)
tEvery dt = tAfterEvery (-inf) dt

evtIs :: Eq evt => evt -> Condition (Timed evt, dom)
evtIs evt = let cnd ((t,e),_) = (e == evt, evtIs evt)
             in Cnd cnd


evtIn :: Eq evt => [evt] -> Condition (Timed evt, dom)
evtIn evts = let cnd ((t,e),_) = (e `elem` evts, evtIn evts)
             in Cnd cnd

cand :: Condition a -> Condition a -> Condition a
cand c1 c2 = Cnd $ \a -> let (r1, c1') = checkCnd c1 a
                             (r2, c2') = checkCnd c2 a
                         in (r1 && r2, cand c1' c2')


cor :: Condition a -> Condition a -> Condition a
cor c1 c2 = Cnd $ \a -> let (r1, c1') = checkCnd c1 a
                            (r2, c2') = checkCnd c2 a
                        in (r1 || r2, cor c1' c2')
infixr 3 &=
(&=) = cand
infixr 2 |=
(|=) = cor

logger :: ((Timed evt) -> dom -> log) -> Logger evt dom log
logger f = Lgr $ \te d -> (f te d, logger f)

logWhen :: Monoid log => Condition (Timed evt,dom) -> Logger evt dom log -> Logger evt dom log
logWhen cnd lgrIn = Lgr lgr' 
        where
            lgr' (t, evt) dom = 
                    case checkCnd cnd ((t,evt),dom) of
                        (True, cnd') -> let (log, lgrIn') = runLogger lgrIn (t,evt) dom
                                        in (log, logWhen cnd' lgrIn')
                        (False, _)   -> (mempty, logWhen cnd lgrIn)



            
