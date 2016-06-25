-- | The final result is a 'Move'. It is computed by refining
-- 'Intermediate's.

{-
data Move = Mov {
            movTime :: Instant,
            movItem :: Item,
            movPos  :: Position
}deriving (Eq, Show)


-- | execute a 'Move' modifying the 'System'
movExec :: Move -> State System ()
movExec (Mov t itm pos) = do
    sys <- get
    itemDb <- gets sysItems
    let itemDb' = itmMove t (itmId itm) pos itemDb
    modify $ (\s -> s{sysItems=itemDb'})
    return ()

-}





{-
-- | Shorthand
itrWait waits events = Intermediate (Left waits) events
-- | Shorthand
itrOkay a events = Intermediate (Right a) events
        

-- | Create Waits for all Source locations
waitAnySrc :: Process -> [Wait]
waitAnySrc prc = map wait (prcSrcLocs prc)
        where
            wait loc = Wait Adding (At loc) (prcId prc)


-- | Get Items at src ports or wait for one to arrive
chkSrcItems :: Process -> ItemDb -> Intermediate [Item]
chkSrcItems prc itemDb = 
        case items of
            [] -> itrWait (waitAnySrc prc) []
            _  -> itrOkay items []
        where
            items = prcSrcItems prc itemDb

-- | Get Items in buffer or wait for one to arrive
chkBuffItems :: Process -> ItemDb -> Intermediate [Item]
chkBuffItems prc itemDb = 
        case items of
            [] -> itrWait [Wait Adding  (Processing pid) pid] []
            _  -> itrOkay items []
        where
            pid = prcId prc
            items = itemsAt 1 (Processing $ prcId prc) itemDb


-- | Check wheter target item has enough space

chkSpace:: PrcId -> Item -> ItemDb -> Item -> Intermediate Item
chkSpace prcId itm db dest
        | itmVolume itm < itmSpace dest db = itrOkay itm [] 
        | otherwise = itrWait [(Wait Removing (top dest) prcId)] []
        where
            top i = case (itmPosition i) of
                        (Inside i') -> top (lookupId i' db)
                        _           -> itmPosition i

type Runner = Process -> Instant -> Reader System (Intermediate Move)



runBeltIn :: Runner
runBeltIn prc@(Prc prcId srcs snks (PrpBelt latency)) t = 
        do -- Reader monad
            itemDb    <- asks sysItems
            processDb <- asks sysProcesses
            myBuffer  <- return $ itmBuffer (Processing prcId) itemDb
            return $ do -- Intermediate monad
                        srcItems <- chkSrcItems prc itemDb
                        chkSpace prcId (head srcItems) itemDb myBuffer
                        return $ Mov t (head srcItems) (Inside $ itmId myBuffer)

runBeltOut :: Runner
runBeltOut prc@(Prc prcId srcs snks (PrpBelt latency)) t = 
        do -- Reader monad
            itemDb    <- asks sysItems
            processDb <- asks sysProcesses
            myBuffer  <- return $ itmBuffer (Processing prcId) itemDb
            myOut     <- return $ itmBuffer (At $ prtLoc $ head snks) itemDb
            return $ do -- Intermediate monad
                -- xxx check latency
                        srcItems <- chkBuffItems prc itemDb
                        chkSpace prcId (head srcItems) itemDb myOut
                        return $ Mov t (head srcItems) (Inside $ itmId myBuffer)

-}


-- ** Misc

safeHead :: [a] -> Maybe a -- xxx needed
safeHead xs = case xs of
                  [] -> Nothing 
                  (x:xs) -> Just x

-- | Randomly shuffle a List xxx - needed?

-- | A lookup which raises an exception when key was not found instead
-- of returning Nothing.
lookupId  id map = fromJust $ M.lookup id map


{-# LANGUAGE BangPatterns #-}
{-|
Module      : Simulation
Description : is the main simulation engine
Copyright   : (c) Martin Drautzburg, 2015
License     : GPL-3
Maintainer  : Martin.Drautzburg@web.de
Stability   : experimental
Portability : POSIX

This module implements 'run' which takes an existing 'SimState' and
projects it into the future until an exit condition is met or the
'EventQu'eue 'events' runs dry.
-}

module Simulation where 

import Text.Printf
import Control.Monad.State.Strict
-- import Debug.Trace
import Data.Monoid
import qualified Data.Heap as H
import Misc.Lens
import Event
import qualified Domain.System as Sys

-- * Entities

type Log = [String]

-- | The current state of the simulation sans the time. It primarily
-- holds the state of the 'System', but additionally
-- Simulation-specific data, namely the Event-Queue
data SimState = Sis {
            events  :: ! (EventQu Int),
            sisSys  :: ! Sys.System

} deriving Show

-- | Lens to run 'System' operations on the (bigger) 'SimState'
onSys :: Lens Sys.System SimState 
onSys (Sis e s) = (s, Sis e)

-- | A condition to stop the Simulation. A Simuation will stop when
-- there are no more Events or when this condition holds. This
-- function has access to the 'Instant' of the next event to be
-- processes and the full 'SimState'. In most cases it is sufficient
-- to check the time.
type ExitP = (Instant, SimState) -> Bool


-- *Running 

-- | Toplevel function to run a Simulation returning the final
-- 'SimState' and a 'Log'.
run :: ExitP -> State SimState Log
run exitp = do
    sis   <- get
    sevts <- gets events

    case H.view sevts of
        Just(evt,evts) -> 
                if exitp (evtTime evt,sis)
                then return [printf "Exit criteria met at t=%f" (evtTime evt)] 
                else do
                    evqs' <-focus onSys $ step evt
                    let sevts' = foldr (<>) evts evqs'
                    modify $ \s -> s{events = sevts'}
                    run exitp
        Nothing -> return ["No more Events"] 


        
-- | Handle a single Event, resulting in a 'System' change and
-- possibly new 'Event's. This tries all Processes who have a chance
-- of responding to the event.
step :: Event Int-> State Sys.System [EventQu Int]
step tev = {-# SCC xxsimStep #-}do
               prcs  <- gets Sys.processList
               evq'  <- mapM (handleEvt tev) prcs
               return evq'



-- | Send a single 'Event' to a single 'RunProcess' and collect its
-- effects
handleEvt :: Event Int-> Sys.RunProcess -> State Sys.System (EventQu Int)
handleEvt tev rpr =
        let (mov, evq, run') = {-# SCC xxhandleEvt #-}Sys.runRunner tev rpr
        in do
            focus Sys.onItemDb $ modify mov
            Sys.setRunner (Sys.rprId rpr) run'
            return evq

-- * Lenses











{-# LANGUAGE BangPatterns #-}
{-|
Module      : Simulation
Description : is the main simulation engine
Copyright   : (c) Martin Drautzburg, 2015
License     : GPL-3
Maintainer  : Martin.Drautzburg@web.de
Stability   : experimental
Portability : POSIX

This module implements 'run' which takes an existing 'SimState' and
projects it into the future until an exit condition is met or the
'EventQu'eue 'events' runs dry.
-}

module Simulation where 

import Control.Monad.State.Strict
import qualified Data.Heap as H
import Event
import qualified Domain.Domain as Dom

-- * Entities

-- ** EventQu

type Log = [String]

-- | Event queue ordered by Instant (TODO: why here)
type EventQu a = H.MinHeap (Event a)

-- | Create an event queue with a single event in it
evqSingle :: Ord a => Event a -> EventQu a
evqSingle evt = H.singleton evt


-- | Add an event to an 'EventQu'eue
evqAdd :: Ord a => Event a -> EventQu a -> EventQu a
evqAdd evt = H.insert evt

-- ** SimState

-- | The current state of the simulation sans the time. It primarily
-- holds the state of the 'System', but additionally
-- Simulation-specific data, namely the Event-Queue
data SimState = Sis {
            events     :: ! (EventQu Int),
            processes  :: ! (Dom.IndexedDb Dom.Process),
            items      :: ! (Dom.IndexedDb Dom.Item)
} deriving Show


-- | A condition to stop the Simulation. A Simuation will stop when
-- there are no more Events or when this condition holds. This
-- function has access to the 'Instant' of the next event to be
-- processes and the full 'SimState'. In most cases it is sufficient
-- to check the time.
type ExitP = Instant -> SimState -> Bool


-- *Running 

{-
-- | Toplevel function to run a Simulation returning the final
-- 'SimState' and a 'Log'.
run :: ExitP -> State SimState Log
run exitp = do
    sis   <- get
    sevts <- gets events

    case H.view sevts of
        Just(evt,evts) -> 
                if exitp (evtTime evt) sis
                then return [printf "Exit criteria met at t=%f" (evtTime evt)] 
                else do
                    evqs' <-focus onSys $ step evt
                    let sevts' = foldr (<>) evts evqs'
                    modify $ \s -> s{events = sevts'}
                    run exitp
        Nothing -> return ["No more Events"] 


        
-- | Handle a single Event, resulting in a 'System' change and
-- possibly new 'Event's. This tries all Processes who have a chance
-- of responding to the event.
step :: Event Int-> State Sys.System [EventQu Int]
step tev = {-# SCC xxsimStep #-}do
               prcs  <- gets Sys.processList
               evq'  <- mapM (handleEvt tev) prcs
               return evq'

-}

-- | Send a single 'Event' to a single 'RunProcess' and collect its
-- effects
handleEvt :: Event Int-> Dom.Process -> State SimState Log
handleEvt evt prc = do
    (Sis evts prcs itms) <- get
    case Dom.runPrc prc evt itms of
        Nothing -> return ["Not interested"]
        Just (mov, itmId, prc') -> let x=1
                                   in return []





