{-# LANGUAGE BangPatterns #-}
{-|
Module      : Des
Description : Combinators for creating 'Logger's
Copyright   : (c) Martin Drautzburg, 2015
License     : GPL-3
Maintainer  : Martin.Drautzburg@web.de
Stability   : experimental
Portability : POSIX

This modules provides some rudimentary combinators to simplify
creating 'Logger's. Its primary purpose is to restrict logging to
certain conditions.

-}

module Logger where
import Des
import Data.Monoid
-- * Condition

-- | Run some check on a and return Nothing when the check
-- fails. Otherwise return a new 'Condition'. This is kind of a
-- condition with state, like a flip-flop.
newtype Condition a  = Cnd {checkCnd :: a ->  Maybe (Condition a)}

-- | Create a stateless 'Condition' from a predicate
cndIf :: (a->Bool) -> Condition a
cndIf p = Cnd cnd
        where
            cnd a
                    | p a = Just $ cndIf p
                    | otherwise = Nothing

-- | Create a 'Condition' which checks 'Timed' Events and which holds
-- when the Timed Event occured before 'Instant'.
tBefore:: Instant -> Condition (Timed evt, dom)
tBefore t0 = let p ((t,_),_) = t < t0
             in cndIf p
            


-- | Create a 'Condition' which checks 'Timed' Events and which holds
-- when the Timed Event occured after 'Instant'.
tAfter:: Instant -> Condition (Timed evt, dom)
tAfter t0 = tAfterEvery t0 0.0

-- | Create a 'Condition' which checks 'Timed' Events and which holds
-- every 'Interval' units of time.

tEvery:: Instant -> Condition (Timed evt, dom)
tEvery dt = tAfterEvery (-inf) dt

-- | Create a 'Condition' which checks 'Timed' Events and which holds
-- when the Timed Event occurred after 'Instant' and then holds every
-- 'Interval' units of time.
tAfterEvery :: Instant -> Interval -> Condition (Timed evt, dom)
tAfterEvery t0 dt = Cnd cnd
        where
            cnd ((t,_),_) 
                    | t >= t0   = Just $ tAfterEvery (t+dt) dt
                    | otherwise = Nothing

-- | Like 'tAfterEvery' but multiply 'Interval' with a factor, leading
-- to ever increasing log intervals.
tAfterEveryExp :: Double -> Instant -> Interval -> Condition (Timed evt, dom)
tAfterEveryExp e t0 dt = Cnd cnd
        where
            cnd ((t,_),_) 
                    | t >= t0   = Just $ tAfterEveryExp e (t+dt) (dt*e)
                    | otherwise = Nothing


-- | Create a 'Condition' which checks 'Timed' Events and which holds
-- when the Event is in the provided list of Events.

evtIn :: Eq evt => [evt] -> Condition (Timed evt, dom)
evtIn evts = let p ((_,e),_) = e `elem` evts
             in cndIf p

-- | Create a 'Condition' from two conditions which holds when both of
-- them hold.
cand :: Condition a -> Condition a -> Condition a
cand c1 c2 = let cnd a = do 
                     c1' <- checkCnd c1 a
                     c2' <- checkCnd c2 a
                     return (cand c1' c2')
             in Cnd cnd

-- | Shorthand for 'cand'
infixr 3 &=
(&=):: Condition a -> Condition a -> Condition a
(&=) = cand


-- | Create a 'Condition' from two conditions which holds when one of
-- them holds.
cor :: Condition a -> Condition a -> Condition a
cor c1 c2 = Cnd cnd
        where
            cnd a =  case (checkCnd c1 a,checkCnd c2 a) of
                         (Nothing,  Nothing)   -> Nothing
                         (Just c1', Nothing)   -> Just (cor c1' c2)
                         (Nothing,  Just c2')  -> Just (cor c1  c2')
                         (Just c1', Just c2')  -> Just (cor c1' c2')

-- | Shorthand for 'cor'
infixr 2 |=
(|=):: Condition a -> Condition a -> Condition a
(|=) = cor

-- * Logger

-- | Create a 'Logger' from a simple function which returns log
-- entries
logger :: ((Timed evt) -> dom -> log) -> Logger evt dom log
logger f = Lgr lgr
        where
            lgr te d = (f te d, logger f)

{- | Restrict a 'Logger' by a 'Condition' such that it only produces
 log entries when the condition holds.

==== __Examples__

> ex_lgr = logWhen logCondition (logger write)
>        where
>            logCondition    = evtIn [Foo,Start] |= tEvery 100000
>            write (t,e) dom = [printf "t=%12.2f %-6s -  New State = %d" t (show e) dom]
>
-}
logWhen :: Monoid log => Condition (Timed evt,dom) -> Logger evt dom log -> Logger evt dom log
logWhen cnd lgrIn = Lgr lgr' 
        where
            lgr' tev dom =  
                    case checkCnd cnd (tev, dom) of
                        (Just cnd') -> let (log', lgrIn') = runLogger lgrIn tev dom
                                       in (log', logWhen cnd' lgrIn')
                        Nothing   -> (mempty, logWhen cnd lgrIn)
