%include lhs2TeX.fmt
%include greek.fmt
%options ghci -fglasgow-exts

\subsubsection{Logger}

A |Logger| creates log entries based on the current Domain-state and
the event which caused the new state. We provide some ways to create
custom |Loggers| easily.

%if False
\begin{code}          

-- | A 'Logger' takes an Event with its 'Instant' plus the Domain in
-- the state /after/ the Event was handled.

        
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
import Control.Monad
import System.TimeIt
import Text.Show.Pretty
import Time
lpp a = do
    putStrLn "\\begin{verbatim}"
    putStrLn $ ppShow a
    putStrLn "\\end{verbatim}"

\end{code}
%endif
        
\begin{code}        
-- | A formatter does the formatting
type Fmtr a log = a -> log
\end{code}

A |Logger| must maintain an internal state, or it would be impossible
to log in certain time-intervals. Hence a |Logger| does not only
return new log-entries, but also a possibly new version of itself.

\begin{figure}[htb!]
\centering
\includegraphics[width=4cm]{Logger.eps}
\end{figure}


\begin{code}
-- | A Logger takes an |a| and an existing log and returns a new log and a new Logger
data Logger a log = Lgr {runLogger :: a -> log -> (log, Logger a log)}
\end{code}

To compose a complex |Loggers| from simple |Loggers|, we make |Logger|
an instance of |Monoid|.This allows combining |Loggers| with |mappend|
or the shorthand |<>| operator.


\begin{code}
instance Monoid (Logger a log) where
        mempty = Lgr (\_ l -> (l,mempty))
        mappend lgr1 lgr2 = Lgr $ \a l ->
                            let !(log1',!lgr1') = runLogger lgr1 a l
                                !(log2',!lgr2') = runLogger lgr2 a log1'
                            in (log2', mappend lgr1' lgr2')
\end{code}        


For |conditional logging| we provide a helper function, which takes
two predicates |dp| and |p|. The |Logger| will not produce any output
before |p| becomes true. Every time a log is written, the predicate
|p| is replaced by a new predicate which is computed by applying |dt|
to the current |a|. Thus the critera when and when not to log are
recomputed each time a log is written.
        
\begin{code}        
logIfP' :: Monoid log => (a->a->Bool) -> (a->Bool) ->  Fmtr a log -> Logger a log
logIfP' dp p fmtr = Lgr f
        where
            f a l = if p a
                    then (fmtr a  <> l, logIfP' dp (dp a) fmtr)
                    else (l,            logIfP' dp p fmtr)
\end{code}                    

A simplified version of the above, which continues logging once the
first log has been written. Not very useful.

\begin{code}                    
logIfP :: Monoid log => (a->Bool) -> Fmtr a log -> Logger a log
logIfP p = logIfP' (const p) p
\end{code}                    

In the same vein, we can start logging after a certain number of calls
|start|. From then on we log at every |dn| invocations.

\begin{code} 
-- | Count calls and write log when calls has reached start and then every dn calls
logCount' :: Monoid log => Int -> Int -> Int ->  Fmtr (Int,a) log -> Logger a log
logCount' dn start calls fmtr = Lgr f
        where
            f a l = if calls == start
                    then (fmtr (calls,a)  <> l, logCount'  dn (start+dn) (calls+1) fmtr)
                    else (l,                    logCount'  dn start      (calls+1) fmtr)


-- | Count calls and write log every dn calls
logCount dn = logCount' dn dn 0
\end{code} 

This logger is more specific. It knows that the |a| is really a pair
|(Timed evt, dom)|, where the types |evt| and |dom| are still
unspecified.

\begin{code} 
-- | log every dt units of time
logEvery :: Monoid log => Double -> Fmtr (Timed evt, dom) log -> Logger (Timed evt, dom) log
logEvery dt fmtr = let p0 ((t,_),_) = t >= dt
                       dp ((t,_),_) ((t',_),_) = t' >= t + dt
                   in logIfP' dp p0 fmtr
\end{code}
    
Example Loggers in action  
\begin{code}


ex_fmtr1 :: Fmtr (Int,a) [String]
ex_fmtr1 (x,_) = ["Counted1 to " ++ (show x)]

ex_fmtr2 :: Fmtr (Int,a) [String]
ex_fmtr2 (x,_) = ["Counted2 to " ++ (show x)]

ex_inputs :: [Int]
ex_inputs = [1..100000]

ex_logger = mempty <> logCount 30000 ex_fmtr1 <> logCount 50000 ex_fmtr2

ex_main = do
    putStrLn "\\begin{verbatim}"
    timeIt $ putStrLn $ ppShow $ testLogger ex_logger ex_inputs
    putStrLn "\\end{verbatim}"
            where
                testLogger :: Logger Int [String] -> [Int] -> [String]
                testLogger lgr xs = fst $ foldl' f ([],lgr) xs
                f (log', lgr') x = runLogger lgr' x log'

\end{code}
\begin{run}
\perform{ex_main}
\end{run}





