{-# LANGUAGE BangPatterns #-}
{-|
Module      : Des
Description : - A discrete event simulation engine
Copyright   : (c) Martin Drautzburg, 2015
License     : GPL-3
Maintainer  : Martin.Drautzburg@web.de
Stability   : experimental
Portability : POSIX

This module establishes the types required for a Des and implements a
'runSim' function, which projects an initial state into the future and
returns the final state.

Des implements the /main loop/ of a simulation. It is not specific
about the Domain it is simulating, nor does it care about the types of
Events which occur or the log it produces as a result. All these are
delegated to domain-specific functions. Hence many types are
paramerized by /evt/, /dom/, /log/ or a subset thereof.

So the ingedients for a Simulation are:

* The state 'SimState' of the simulation. An initial state must be
passed to the simulation. It consists of

    * A Log which accumulates log entries.

    * A Domain, which repensts the state of the system we're simulating. 

    * An Event-queue 'EventQu'. 

and

* The behaviour 'SimBehaviour' of the simulation, which is described by

    * A 'Logger', which can produce new log entries whenever an Event was handled.

    * A 'Handler', which receives an Event and the current Domain
          and produces a new Domain and possibly schedules additional Events.

    * An exit condition 'ExitP', which returns true when the simulation shall end.


The __performance__ of the simulation is determined by the performance
of the 'Logger' and the 'Handler' (assuming the 'ExitP' is reasonably
fast). With a very simple handler and logger, 'runSim' can consume and
create one million Events in less than 0.2 seconds.

The __size__ of this module is 29 lines of code (without comments and blank lines)

-}

module Des where
import Logger
import qualified Data.Heap as H
import Data.Monoid
import Prelude hiding (log)
-- import Debug.Trace

-- * Time

-- | A point in time
type Instant = Double

-- | Difference between two 'Instant's
type Interval = Double

-- | Infinitly long 'Interval' or distant future
inf :: Double
inf = 1/0 

-- | Something which is associated with an 'Instant'
type Timed a = (Instant, a)

-- * State
-- | The ingredients which describe the initial (and running) state of the simulation
type SimState evt dom log = (log, dom, EventQu evt)


-- | The EventQu holds Events along with their 'Instant's. The type of
-- the Events themselves is not relevant for the simulation. However,
-- the 'Logger', the 'Handler' and the 'ExitP' are specific about the
-- Events they accept as input.
type EventQu evt = H.MinHeap (Timed evt)

-- * Behaviour

-- | The three ingredients which describe the behavior of the simulation
type SimBehaviour evt dom log = (Logger (Timed evt, dom) log, Handler evt dom,ExitP evt dom )


-- | A Logger takes an Event with its 'Instant' plus the Domain in
-- the state /before/ the Event gets handled. See "Logger" for details.

-- | log every dt units of time
logEvery :: Monoid log => Double -> Wtr (Timed evt, dom) log -> Logger (Timed evt, dom) log
logEvery dt wtr = let p0 ((t,_),_) = t >= dt
                      nxt ((t,_),_) ((t',_),_) = t' >= t + dt
                  in logIfStep p0 nxt wtr


-- | A Handler takes an Event and a current Domain and produces 
--
-- * A new 'EventQu' which hold Events it schedules itself. 
-- * a new Domain whose state is possibly altered, 
-- * a new version of itself. The latter is required for Handlers
-- which e.g. limit throughput. Once such a handler has handled an
-- Event it will not handle further Events for some time, i.e. its own
-- state has been altered.
--
-- A Handler is free to ignore an Event. In that case it returns Nothing.

newtype Handler evt dom = Hdr {runHandler :: Timed evt -> dom -> (EventQu evt, dom, Handler evt dom)}

-- | The exit condition can respond to the 'Instant', the Event or the
-- Domain or a combination thereof. You can however, not stop the
-- simulation after a specified number of steps, as we do not keep
-- track of the number of steps.
type ExitP evt dom = (Timed evt, dom) -> Bool


-- * runSim

-- | This is the main simuation function. It takes a 'SimBehaviour' and
-- an initial 'SimState'. The result of the simulation is the final
-- 'SimState'

runSim :: (Ord evt, Monoid log) => SimBehaviour evt dom log -> SimState evt dom log -> SimState evt dom log
runSim (lgr, hdr, xtp) (!log,!dom,!evq)  =
        case step of
            Nothing -> (log, dom, evq) -- end of simulation
            Just (newEvq, newDom, newHdr, newLgr, newLog) -> runSim (newLgr,newHdr,xtp) (newLog,newDom,newEvq)
        where 
            -- check for end conditions or run handler
            step = do
                (evt, evts) <- H.view evq -- no more Events -> Nothing
                if xtp (evt,dom)  then Nothing
                else
                        let (log',lgr')        = runLogger lgr (evt,dom) log
                            (evq', !dom', hdr') = runHandler hdr evt dom
                        -- append new event and new log entries
                        in return (evq'<>evts, dom', hdr', lgr', log')



