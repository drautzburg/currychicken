{-|
Module      : Time
Description : implements 'Instant' and 'Interval'
Copyright   : (c) Martin Drautzburg, 2015
License     : GPL-3
Maintainer  : Martin.Drautzburg@web.de
Stability   : experimental
Portability : POSIX

An 'Instant' is a point in time and an 'Interval' is the difference
between tow Instants. 

-}

module Misc.Time where

-- * Entities

-- | A moment in time
type Instant = Double

-- | The difference between two 'Instant's
type Interval = Double

-- | Something associated with an Instant
type Timed a = (Instant, a)

-- * Constants

-- | An infinitely long 'Interval' or an 'Instant' in the distant
-- future (-inf stands for distant past).
inf :: Double
inf = 1/0 



