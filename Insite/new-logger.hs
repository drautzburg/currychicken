-- This logger is a bit faster. The overhead is < 10usec. The normal
-- loggers costs 1 usec, but its current design exposes the internal
-- states of the loggers, such than runSim would need a LogState in
-- its type.

{-# LANGUAGE BangPatterns #-}
module Logger where
import Control.Monad.Writer
import Text.Printf
import Debug.Trace
import Text.Show.Pretty
import System.TimeIt


pp x = putStrLn $ ppShow x

data LogState a = Init | Currently a
type Wtr a l = a -> l



type Logger s a log = a -> (log, LogState s) -> (log, LogState s) 

logByCall :: Monoid log => Int -> Wtr Int log -> Logger (Int,Int) Int log
logByCall dx wtr a (l, Currently (x0,xi))  = if xi==x0
                                             then (wtr xi <>l, Currently (x0+dx, xi+1))
                                             else (l, Currently (x0, xi+1))
logByCall dx wtr a (l, Init) = logByCall dx wtr a (l, Currently (0,0))




logByPx :: Monoid log => (a->a->Bool) -> Wtr a log -> Logger (a->Bool) a log
logByPx dp wtr a (l,(Currently p0)) = if p0 a
                                      then (wtr a <> l, Currently (dp a))
                                      else (l, Currently p0)
logByPx dp wtr a (l,Init) = logByPx dp wtr a (l, Currently $ const True)


mapLogger :: Logger s a l -> [a] -> (l,LogState s) ->  (l,LogState s)
mapLogger wtr [] ls0 = ls0
mapLogger wtr (x:xs) ls0 = let !ls' = wtr x ls0
                            in mapLogger wtr xs ls'

combineLogger :: Logger s1 a l -> Logger s2 a l -> Logger(s1,s2) a l
combineLogger wtr1 wtr2 a (l, Currently (s1,s2))   = let !(l1', Currently s1') = wtr1  a (l,   Currently s1)
                                                         !(l2', Currently s2') = wtr2  a (l1', Currently s2)
                                                     in  (l2', Currently (s1', s2'))

combineLogger lgr1 lgr2 a (l,Init)   = let !(l1', Currently s1') = lgr1  a (l,  Init)
                                           !(l2', Currently s2') = lgr2  a (l1',Init)
                                       in  (l2', Currently (s1', s2'))

combined a (l, Currently (s1,s2,s3)) = 
        let 
                lgr1 = logByCall 1000000               (\i -> [printf "%d invocations" i])
                lgr2 = logByCall 20000000              (\i -> [printf "** %d invocations" i])
                lgr3 = logByPx (\x1-> (> x1+750000))   (\a -> [printf "Predicate x=%d" a, "with a next log"])
                !(l1', s1') = lgr1 a (l,s1)
                !(l2', s2') = lgr2 a (l1',s2)
                !(l3', s3') = lgr3 a (l2',s3)
        in (l3',Currently (s1',s2',s3'))

(<+>)  = combineLogger


main = let lgr = logByCall 1000000                   (\i -> [printf "%d invocations" i])
                 <+> logByCall 20000000              (\i -> [printf "** %d invocations" i])
                 <+> logByPx (\x1-> (> x1+750000))   ((\a -> [printf "Predicate x=%d" a, "with a next log"]) :: Wtr Int [String])
       in timeIt $ pp $ fst $ mapLogger lgr [1..10000000] (["Start"], Init)


-- a bit faster
-- main = timeIt $ pp $ fst $ mapLogger combined [1..10000000] (["Start"], (Currently (Init,Init,Init,Init)))
