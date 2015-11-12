{-|
Module      : Domain
Description : The data which is altered by the Simulation
Copyright   : (c) Martin Drautzburg, 2015
License     : GPL-3
Maintainer  : Martin.Drautzburg@web.de
Stability   : experimental
Portability : POSIX

The data which is altered by the Simulation.
-}

module Logistics.Domain where

import Des
import qualified Data.Map as M
import Control.Monad.State.Strict
import Misc.Lens


type Id = Int
-- | A DB whose elements can be looked up by an 'Id'
type IndexedDb a = M.Map Id a

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- 
-- ** Item

-- | Items include mail-itmes, containers, staff and whatnot. Most
-- items move around, with the exception of buffers which are
-- immobile.

data Item = Itm {
            label    :: String,    -- ^ Address or other label
            volume   :: Double,    -- ^ Volume of the Item
            capacity :: Double,    -- ^ Available space inside the Item
            position :: Position,
            arrived  :: Instant    -- ^ When the Item arrived at its Position
} deriving (Eq, Show)

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- 
-- *** Position

-- | Most Items are /In/ another Items. Toplevel Items (typically
-- Buffers) are /At/ a Location.
data Position = 
        AtLoc Id | 
        InItm Id 
               deriving (Eq,Ord,Show)

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- 
-- * Creating Indexed elements

-- | Assign an Id and insert something into an IndexedDb
ins :: a -> State (Int, (IndexedDb a)) Int
ins a = do
    (i,db) <- get
    let i' = i+1
    put (i', M.insert i' a db)
    return i'


-- | Insert a 'Process' into the 'System', giving it a unique 'Id'
insertPrc :: Process -> State (Int, System) Int
insertPrc = (focus onPrcDbSequence) . ins

-- | Insert an 'Item' into the 'System', giving it a unique 'Id'
insertItm :: Item -> State (Int, System) Int
insertItm = (focus onItmDbSequence) . ins


