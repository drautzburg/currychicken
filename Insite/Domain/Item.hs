{-|
Module      : Item
Description : is about 'Items' and their 'Position's
Copyright   : (c) Martin Drautzburg, 2015
License     : GPL-3
Maintainer  : Martin.Drautzburg@web.de
Stability   : experimental
Portability : POSIX

Items fall in two categories. /Space-like/ items are either 'At'
a 'Location' or 'Processing' through a 'Process'. These don't move
around, but just provide space.

/Time-like/ items are either 'Inside' a space-like item or inside
another time-like item. They typically move around as time goes
by.

-}

module Domain.Item where

import Misc.Time
import Domain.Location
import Domain.Process

-- * Entities

type Volume      = Double
type Capacity    = Double
type ItmId       = Int
type ItmCapacity = Capacity
type ItmVolume   = Volume
type ItmLabel    = String

-- | Items fall in two categories. /Space-like/ items are either 'At'
-- a 'Location' or 'Processing' via a 'Process'. These don't move
-- around, but just provide space.
--
-- /Time-like/ items are either 'Inside' a space-like item or inside
-- another time-like item. They typically move around as time goes
-- by.

data Item = Itm {
            -- | Everything we know about the item (format, class, destination)
            itmLabel    :: ItmLabel, 
            -- | the volume of the 'Item' itself
            itmVolume   :: ItmVolume,
            -- | how much can be packed inside the 'Item'
            itmCapacity :: ItmCapacity,
            -- | where the item is
            itmPosition :: Position,
            -- | time when the item arrived at its current 'Position'
            itmArrived  :: Instant,
            itmId       :: ItmId
} deriving (Eq, Show)


-- | Position generalizes 'Location' and caters for packed and
-- processing Items

data Position = 
        -- | space-like
        At LocId | 
        -- | space-like
        Processing PrcId |
        -- | time-like, moving 'Item'
        Inside ItmId 
                        deriving (Eq,Ord,Show)
