{-# LANGUAGE BangPatterns #-}
{-|
Module      : Logger
Description : - Logger module for "Des"
Copyright   : (c) Martin Drautzburg, 2015
License     : GPL-3
Maintainer  : Martin.Drautzburg@web.de
Stability   : experimental
Portability : POSIX

A Logger takes an __a__ and produces

* one or more log entries. New log entries are /prepended/, so the 
/first/ log-entry is the one that was produced /last/.
* a new version of itself. The latter is required, so you can
e.g. log at fixed time-intervals. 


-}
module Logger where
import Data.List
import Data.Monoid
import System.TimeIt
import Text.Show.Pretty

-- | A writer does the formatting
type Wtr a log = a -> log

-- | A looger is a writer plus an internal state
data Logger a log = Lgr {runLogger :: a -> log -> (log, Logger a log)}

instance Monoid (Logger a log) where
        mempty = Lgr (\_ l -> (l,mempty))
        mappend lgr1 lgr2 = Lgr $ \a l -> let !(log1',!lgr1') = runLogger lgr1 a l
                                              !(log2',!lgr2') = runLogger lgr2 a log1'
                                          in (log2', mappend lgr1' lgr2')


logIfP' :: Monoid log => (a->a->Bool) -> (a->Bool) ->  Wtr a log -> Logger a log
logIfP' dp p wtr = Lgr f
        where
            f a l = if p a
                    then (wtr a  <> l, logIfP' dp (dp a) wtr)
                    else (l,           logIfP' dp p wtr)

logIfP :: Monoid log => (a->Bool) -> Wtr a log -> Logger a log
logIfP p = logIfP' (const p) p
            

-- | Count calls __s__ and write log when s has reached nxt and then every dn calls
logCount' :: Monoid log => Int -> Int -> Int ->  Wtr (Int,a) log -> Logger a log
logCount' dn nxt s wtr = Lgr f
        where
            f a l = if s == nxt
                    then (wtr (s,a)  <> l, logCount'  dn (nxt+dn) (s+1) wtr)
                    else (l,               logCount'  dn nxt      (s+1) wtr)


-- | Count calls and write log every dn calls
logCount dn = logCount' dn dn 0



-- testLogger :: Logger Int Int [String] -> [String]
testLogger lgr xs = fst $ foldl' f ([],lgr) xs
        where
            f (log', lgr') x = runLogger lgr' x log'

ex_wtr :: Wtr (Int,a) [String]
ex_wtr (x,_) = ["Counted to " ++ (show x)]

ex_wtr2 :: Wtr Int [String]
ex_wtr2 x = ["Counted to " ++ (show x)]

ex_inputs :: [Int]
ex_inputs = [1..10000000]

ex_logger = mempty <> logCount 300000 ex_wtr <> mempty
-- ex_logger = logCount 300000 ex_wtr 


ex_main = do
    timeIt $ putStrLn $ ppShow $ testLogger ex_logger ex_inputs

-- main = ex_main


