%include lhs2TeX.fmt
%include greek.fmt
%options ghci -fglasgow-exts

%if False
\begin{code}
{-# LANGUAGE BangPatterns#-}
module Des where
import qualified Data.Heap as H
import Time
import Logger
import Handler
import Debug.Trace
\end{code}
%endif

\subsection{Des}
At any point in time, the state of the Simulation is described by the following four things:


\begin{code}
data SimState e d l = SimState {
  hds :: HandlerState e d,
  hdr :: Handler e d,

  lgs :: l,
  lgr :: Logger (Timed (e,d)) l
  }
\end{code}

The HandlerState and the the LoggerState are 'public' and possibly of
interest to others, while the Handler and the Logger themselves, may
keep their own states, but they won't tell anybody about it.


\begin{code}
-- | Exit predicate - when to end the Simuation
type ExitP e d  = Timed e -> d -> Bool
\end{code}

Finally the Simulation as such\footnote{The many exclamation marks are
\emph{bang patterns} which enforce a full evaluation of its
argument. Haskell is by default a lazy language, but its laziness gets
in the way here and increases memory consumption}.

\begin{code}
runSim :: (Ord e, Show e) =>
          ExitP e d -> SimState e d l -> SimState e d l
\end{code}
\needspace{20em}
\begin{code}
-- | Run the simulation
runSim xtp state =  case (H.view . iev . hds) state of
  Nothing -> state -- no more events
  Just ((t,e), tevts)
          -> let -- handler and logger-states
                 !hs = (hds state){iev=tevts}
                 !ls = (lgs state)

                 -- extract the domain
                 !d  = dom . hds $ state

                 -- run handler and logger
                 !(!hds', !hdr')  = (runHandler . hdr) state (t,e) hs
                 !(!lgs', !lgr')  = (runLogger  . lgr) state (t,(e,d)) ls

                 -- assmble new state
                 !state'          = SimState hds' hdr' lgs' lgr'

             -- decide whether to continue
             in if xtp (t,e) (dom hds')
                then  state'
                else  runSim xtp state'
\end{code}        
