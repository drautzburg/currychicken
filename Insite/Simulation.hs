{-# LANGUAGE BangPatterns #-}
{-|
Module      : Simulation
Description : is the main simulation engine
Copyright   : (c) Martin Drautzburg, 2015
License     : GPL-3
Maintainer  : Martin.Drautzburg@web.de
Stability   : experimental
Portability : POSIX

This module implements 'simRun' which takes an existing 'SimState' and
projects it into the future until an exit condition is met or the
'EventQu'eue 'sisEvts' runs dry.
-}

module Simulation where 

import Text.Printf
import Control.Monad.State.Strict
-- import Debug.Trace
import Data.Monoid
import qualified Data.Heap as H

import Misc.Lens
import Misc.Time
import Event
import Domain.System

-- * Entities

type Log = [String]

-- | The current state of the simulation sans the time. It primarily
-- holds the state of the 'System', but additionally
-- Simulation-specific data, namely the Event-Queue
data SimState = Sis {
            sisEvts :: ! EventQu,
            sisSys  :: ! System

} deriving Show

-- | The condition to stop the Simulation. A Simuation will stop when
-- there are no more Events or when this condition holds. This
-- function has access to the 'Instant' of the next event to be
-- processes and the full 'SimState'. In most cases it is sufficient
-- to check the time.
type ExitP = Timed SimState -> Bool


-- *Running 

-- | Toplevel function to run a Simulation returning the final
-- 'SimState' and a 'Log'.
simRun :: ExitP -> State SimState Log
simRun exitp = do
    sis   <- get
    sevts <- gets sisEvts

    case H.view sevts of
        Just((t,evt),evts) -> 
                if exitp (t,sis)
                then return [printf "Exit criteria met at t=%f" t] 
                else do
                    evqs' <-focus onSys $ simStep (t,evt)
                    let sevts' = foldr (<>) evts evqs'
                    modify $ \s -> s{sisEvts = sevts'}
                    simRun exitp
        Nothing -> return ["No more Events"] 


        
-- | Handle a single Event, resulting in a 'System' change and
-- possibly new 'Event's. This tries all Processes who have a chance
-- of responding to the event.
simStep :: Timed Event -> State System [EventQu]
simStep tev = {-# SCC xxsimStep #-}do
    prcs  <- gets sysProcessList
    evq'  <- mapM (simHandleEvt tev) prcs
    return evq'



-- | Send a single 'Event' to a single 'RunProcess' and collect its
-- effects
simHandleEvt :: Timed Event -> RunProcess -> State System EventQu
simHandleEvt tev rpr =
        let (mov, evq, run') = {-# SCC xxsimHandleEvt #-}runRunner tev rpr
        in do
            focus onItemDb $ modify mov
            sysSetRunner (rprId rpr) run'
            return evq

-- * Lenses

-- | Lens to run 'System' operations on the (bigger) 'SimState'
onSys :: Lens System SimState 
onSys (Sis e s) = (s, Sis e)

