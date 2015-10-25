{-|
Module      : Location
Description : is about points
Copyright   : (c) Martin Drautzburg, 2015
License     : GPL-3
Maintainer  : Martin.Drautzburg@web.de
Stability   : experimental
Portability : POSIX

A 'Location' stands for a point on the face of this
earth. Toplevel (/space-like/) Items are at a certain 'Location' or
inside a Process.

'Location' iself does not have a capacity (it is just a point).
Hence you need at least one Item at a 'Location' to make 'Process'
work. This 'Item' stands for the available space there and is
typically never moved. 

Similar considerations apply for the space inside a
Process. E.g. for a road transport, this item stands for the road
capacity and not for the capacity of the truck.


-}

module Domain.Location where

import qualified Data.Map as M

-- * Entities

type Id = Int
type LocId = Id


data Location = Loc {
            locId :: LocId,
            locName :: String
        } deriving (Eq, Show)

-- | A Collection of Locations, e.g. all Locations of the 'System'
type LocationDb = M.Map LocId Location

