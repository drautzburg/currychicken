{-|
Module      : Port
Description : is about points
Copyright   : (c) Martin Drautzburg, 2015
License     : GPL-3
Maintainer  : Martin.Drautzburg@web.de
Stability   : experimental
Portability : POSIX

Ports are Sources or Sinks of a Process and refer to a
'Location'. PortName must be unique within a Process, but not
globally. 

A Port is associated with a degree of unnesting. "Put
items inside the space at loc" would have unNest=1 and is
appropriate for transport-like Processes. Pack or unpack Processes
typically have unNest=2.
-}
module Domain.Port where

import Domain.Location

-- * Entities

type UnNest = Int
type PortName = String


type Port = (PortName, UnNest, LocId)

-- * Accessing

-- | get the locId of a 'Port'
prtLoc :: Port -> LocId
prtLoc (_,_,pid) = pid


