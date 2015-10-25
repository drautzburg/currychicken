
{-|
Module      : Event
Description : implements the possible Events
Copyright   : (c) Martin Drautzburg, 2015
License     : GPL-3
Maintainer  : Martin.Drautzburg@web.de
Stability   : experimental
Portability : POSIX

There is a limited set of Evemts the 'Runner's of the simulation can
handle. These are defined here
-}


module Event where

import qualified Data.Heap as H
import Misc.Time

-- * Entities

data Event = 
        EvtAdd  Int   |
        EvtRem  Int   |
        EvtStop Int   |
        EvtTick Int
             deriving (Eq, Ord, Show)

-- | Event queue ordered by Instant (TODO: why here)
type EventQu = H.MinHeap (Timed Event)

-- * Initializing

-- | Create an event queue with a single event in it
evqSingle :: Timed Event-> EventQu
evqSingle tev = H.singleton tev

-- * Adding

-- | Add an event to an 'EventQu'eue
evqAdd :: Timed Event -> EventQu -> EventQu
evqAdd tev = H.insert tev

