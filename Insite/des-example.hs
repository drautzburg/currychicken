{-# LANGUAGE BangPatterns#-}
import Time
import Logger
import Handler
import Des
        
import Data.Monoid
import qualified Data.Heap as H
import System.TimeIt
import Text.Printf

data EvtType = Start|Step|Mesg deriving (Eq,Ord,Show)

ex_handler :: Handler EvtType Int
ex_handler = Hdr hdr
        where
            hdr (t,e) (!Hds !ie !xe !d) =
                    let (ie', d') = case e of
                                        Start -> (H.insert (t,   Step) ie, d)
                                        Step  -> (H.insert (t+1, Step) ie, d+1)
                                        _     -> (ie,d) 
                    in ((Hds ie' xe d'), Hdr hdr)


ex_lgr :: Logger (Timed (EvtType, Int)) String
ex_lgr = loggers [
          logger (logIf isMesg) (Fmt fmt1),
          logger (logEveryN 100000 0) (Fmt fmt2),
          logger (logEveryT 30000 0) (Fmt fmt3) 
          ]
        where
            fmt1 (t,(evt,dom))     = [printf "t=%12.2f - evt=%-4s received at state=%d" t (show evt) dom]
            fmt2 (t,(evt,dom))     = [printf "t=%12.2f - Processed another bunch of events" t]
            fmt3 (t,(evt,dom))     = [printf "t=%12.2f - Time to write some logs" t]
            isMesg (t,(evt,dom)) = evt == Mesg


ex_xtp :: ExitP EvtType Int
ex_xtp (t,_) _ = t>1000000.0

ex_log0 :: LoggerState (Timed String)
ex_log0 = Lgs [(0.0, "Welcome to insite")] 


ex_evq0 :: TimedQ EvtType
ex_evq0 = H.fromList [
           (0,Start),
           (10,Mesg),
           (100000,Mesg)
          ]

ex_dom0 :: Int
ex_dom0 = 0 


run :: IO ()
run = let finalState = runSim ex_xtp (
                                      SimState
                                      (Hds ex_evq0 H.empty ex_dom0)
                                      ex_handler
                                      (Lgs [])
                                      ex_lgr
                                     )
      in do
          mapM_ putStrLn $ (getLog . lgs) finalState
--          putStrLn $ "Final domain = " ++ (show $ d)
--          putStrLn $ "Final queue  = " ++ (show $ H.toList q)
          putStrLn "done"


main :: IO ()
main = timeIt run


