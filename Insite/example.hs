import Data.Monoid
import Control.Monad.State.Strict
import Text.Printf
import Text.Show.Pretty
import System.TimeIt

import qualified Simulation as Sim

import qualified Domain.System as Sys
import qualified Domain.Process as Prc
import qualified Domain.Item as Itm
import Event
import Misc.Time


-- | An example SimRun
exSimRun :: (Sim.Log, Sim.SimState)
exSimRun = let initialEvents = foldr (\t -> evqAdd (t,EvtTick 2)) mempty [1..1]
               exitp (t,sis) = t >= 1000000
           in runState (Sim.run exitp) (Sim.Sis initialEvents exSys )

main :: IO ()
main = timeIt $ do
           mapM_ (pp . show) (fst exSimRun)

-- | Example system: a loop of Belts with items at the Location of the first 
exSys :: Sys.System
exSys = snd $ execState ops (1, Sys.new)
        where
            -- set limits
            nPrcs = 3
            nParcels = 2

            -- shorthand for creating ports, belts and parcels
            pin  loc = [("in",  1, loc)] -- create Port with unNest=1 at loc
            pout loc = [("out", 1, loc)]

            -- create "Belt" Processes
            mkBelt locIdIn locIdOut = \pid -> let prc = Prc.Prc (pin locIdIn) (pout locIdOut) pid
                                              in (prc, exRunPrc 1 prc)
            mkBelt' locId = mkBelt locId (locId+1)

            -- Create Items to move at locId=1
            mkParcel (x, parent) = Itm.Itm ("parcel-CTY="++ printf "%04d" (x::Int) ) 10 100 (Itm.Inside parent) 0
            
            -- building the system
            ops :: State (Int, Sys.System) ()
            ops = do
                prcIds <- mapM (Sys.insertPrc . mkBelt') [1..nPrcs]
                (id,sys) <- get
                let locs = Sys.portLocations sys
                -- close the loop of belts (does not introduce a new loc)
                prcId <- Sys.insertPrc (mkBelt (maximum locs)  (minimum locs)) 
                -- give all Processes a buffer
--                mapM_ (Sys.createInternalBuffer 100) (prcId:prcIds)
                -- give all Locations of Ports a buffer
                locBuffers <- mapM (Sys.createLocBuffer 100) locs
                -- add nParcels parcels to all the loc buffers
                mapM_ (Sys.insertItm . mkParcel) [(p,l) |  l <- locBuffers, p <-[1..nParcels]]

                return ()



-- | An example runner with an internal Int state 
--
-- data Runner = Run (Timed Event -> (ItemDb->ItemDb, EventQu, Runner))
exRunPrc :: Int -> Prc.Process -> Sys.Runner
exRunPrc x prc = Sys.Run run
        where
            run :: Timed Event -> (Sys.ItemDb->Sys.ItemDb, EventQu, Sys.Runner)
            run (t,evt) 
                    | evt == (EvtTick $ Prc.prcId prc) = 
                            let evts = evqSingle (t+1, EvtTick 2) 
                            in (id, evts, exRunPrc (x+1) prc)
                                
                    | otherwise = (id, mempty, exRunPrc x prc )


pp x = putStrLn $ ppShow x
