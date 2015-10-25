{-|
Module      : Process
Description : implements a general 'Process' entity
Copyright   : (c) Martin Drautzburg, 2015
License     : GPL-3
Maintainer  : Martin.Drautzburg@web.de
Stability   : experimental
Portability : POSIX

This module implements a process with 'Port's. it does /not/ implement
the behavior of a process. This is implements via 'Runner' in the
'System' module.
-}

module Domain.Process where

import Domain.Location
import Domain.Port

-- * Entities

type PrcId = Int

-- | All Processes have Sources and Sinks. The process-specific
-- aspects are captured in their 'PrcRunner's.
data Process = Prc {
            prcSources  :: [Port],
            prcSinks    :: [Port],
            prcId       :: PrcId
} deriving (Show)


-- * Accessing

-- | Return the locations of the Src Ports as a List
prcSrcLocs :: Process -> [LocId]
prcSrcLocs prc = map prtLoc $ prcSources prc

-- | Return the locations of the Snk Ports as a List
prcSnkLocs :: Process -> [LocId]
prcSnkLocs prc = map prtLoc $ prcSinks prc

