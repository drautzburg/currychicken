import Des
import Logger
import Data.Monoid
import qualified Data.Heap as H
import System.TimeIt
import Text.Printf

data EvtType = Start|Step|Mesg deriving (Eq,Ord,Show)

ex_hdr :: Handler EvtType Int
ex_hdr = Hdr hdr
        where
            hdr (t,Start) d = (H.singleton (t+1, Step), d,   Hdr hdr)
            hdr (t,Step)  d = (H.singleton (t+1, Step), d+1, Hdr hdr)
            hdr (_,_) d =     (H.empty,                 d,   Hdr hdr)


ex_lgr :: Logger (Timed EvtType, Int) [String]
ex_lgr =  (logCount 100000) writer2 
           <> logIfP isMesg writer1
           <> logEvery 150000 writer3
        where
            writer1 ((t,e),dom)      = [printf "t=%12.2f - evt=%-4s received at state=%d" t (show e) dom]
            writer2 (n,((t,_),_))    = [printf "t=%12.2f - Processed 100000 events" t]
            writer3 ((t,_),_)        = [printf "t=%12.2f - Time to write some logs" t]
            isMesg ((t,evt),dom) = evt == Mesg

ex_xtp :: ExitP EvtType Int
ex_xtp ((t,_), _) = t>1000000

ex_log0 :: [String]
ex_log0 = ["Welcome to insite"] 

ex_evq0 :: EventQu EvtType
ex_evq0 = H.fromList [
           (0,Start),
           (10,Mesg),
           (10000,Mesg)
          ]

ex_dom0 :: Int
ex_dom0 = 0 

run :: IO ()
run = let (l,d,q) = runSim (ex_lgr,ex_hdr,ex_xtp) (ex_log0,ex_dom0,ex_evq0)
      in do
          mapM_ putStrLn l
          putStrLn $ "Final domain = " ++ (show $ d)
          putStrLn $ "Final queue  = " ++ (show $ H.toList q)


main :: IO ()
main = timeIt run

