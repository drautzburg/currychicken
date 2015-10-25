{-|
Module      : System
Description : captures the state of the world
Copyright   : (c) Martin Drautzburg, 2015
License     : GPL-3
Maintainer  : Martin.Drautzburg@web.de
Stability   : experimental
Portability : POSIX

The 'System' is a collection of 'RunProcess'es and 'Item's, where a
RunProcess is a Process plus a 'Runner'

-}

module Domain.System where

import qualified Data.Map as M
import Control.Monad.State.Strict
import Data.List
import Data.Monoid
import Data.Maybe
import Domain.Process
import Domain.Item
import Misc.Time
import Misc.Lens
import Domain.Port
import Domain.Location
import Event

-- * Entities
data System = Sys {
            sysProcesses :: ! ProcessDb,
            sysItems     :: ! ItemDb
} deriving Show


-- | a Runner adds behavior to a Process
data Runner = Run (Timed Event -> (ItemDb->ItemDb, EventQu, Runner))
instance Show Runner where show _ = "a runner"

-- | A Process with behavior
type RunProcess = (Process, Runner)

-- | A Collection of running Processes, e.g. all Processes of the 'System'
type ProcessDb = M.Map Int RunProcess

-- | A Collection of Items, e.g. all Items of the 'System'
type ItemDb = M.Map ItmId Item


-- * Initializing

-- | Create a new system without any 'Process'es or 'Item's
sysNew :: System
sysNew = Sys mempty mempty

-- * Accessing

rprId :: (Process, Runner) -> PrcId
rprId = prcId . fst

-- | Set the runner for a Process with a given PrcId
sysSetRunner :: PrcId -> Runner -> State System ()
sysSetRunner pid run' = do
    prcs <- gets sysProcesses
    let (prc, _) = fromJust $ M.lookup pid prcs
    modify $ sysAddPrc (prc, run')
    return ()


-- | Get all Processes as a List
sysProcessList :: System -> [RunProcess]
sysProcessList = M.elems . sysProcesses

-- | Answer the 'Location's of all 'Port's of all 'Process'es in the
-- 'System'
sysPortLocations :: System -> [LocId]
sysPortLocations sys = nub $ sort $ do
        (prc,_) <- (M.elems . sysProcesses) sys
        srcPort <- prcSources prc
        snkPort <- prcSinks prc
        [prtLoc srcPort, prtLoc snkPort]


-- * Adding

-- | Add a 'Process' to a 'ProcessDb'
sysAddPrc :: RunProcess -> System -> System
sysAddPrc (prc,run) sys =  sys{
                        sysProcesses = M.insert (prcId prc) (prc,run) (sysProcesses sys)
}

-- | Give Process a unique id and add it to the 'System'
sysInsertPrc :: (PrcId -> RunProcess) -> State (Int, System) PrcId
sysInsertPrc mkRpr = do
    (sid, sys) <- get
    let pid  = sid+1
        rpr' = mkRpr pid
        sys' = sysAddPrc rpr' sys
    put (pid, sys')
    return pid


-- | Add an 'Item' to an 'ItemDb'
sysAddItm :: Item -> System -> System
sysAddItm item sys = sys{
                         sysItems = M.insert (itmId item) item (sysItems sys)
                     }

-- | Give Item a unique id and add it to the 'System'
sysInsertItm :: (ItmId->Item) -> State (Int, System) ItmId
sysInsertItm mkItem = do
    (sid, sys) <- get
    let sid' = sid+1
        itm' = mkItem sid'
        sys' = sysAddItm itm' sys
    put (sid', sys')
    return sid'

-- | Add an internal Buffer of a Process to the 'System'
sysCreateInternalBuffer :: Capacity -> PrcId -> State (Id, System) PrcId
sysCreateInternalBuffer cap pid = 
        sysInsertItm  $ Itm "Internal Buffer" 0 cap (Processing pid) (-inf)

-- | Add an internal Buffer at a Location to the 'System'
sysCreateLocBuffer :: Capacity -> LocId -> State (Id, System) ItmId
sysCreateLocBuffer cap lid = 
        sysInsertItm $ Itm "Location Buffer" 0 cap (At lid) (-inf)


-- * Running

-- | Send an 'Event' to a 'Process' 
runRunner :: Timed Event -> RunProcess -> (ItemDb->ItemDb, EventQu, Runner) 
runRunner tev rpr = run tev
        where
            (_, Run run) = rpr

-- * Lenses

-- | -- | Lens to run 'ItemDb' operations on the (bigger) 'System'
onItemDb :: Lens ItemDb System
onItemDb (Sys ps is) = (is, Sys ps)
