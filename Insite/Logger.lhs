%include lhs2TeX.fmt
%include greek.fmt
%options ghci -fglasgow-exts

%if False
\begin{code}
{-# LANGUAGE BangPatterns#-}
module Logger where
import Time
import Debug.Trace
import Control.Monad.Writer
import Data.Machine.Mealy
\end{code}
%endif

\subsection{Logger}

The purpose of a |Logger| is to write interesting things to a log. We
abandoned the idea of doing fine-grained logging so we can do
\emph{any} analysis later on. If running the simulation was costly and
running the analyses was cheap, this would have made sense. But it
turned out that the Simulation is pretty fast and writing lots of
fine-grained data is costly.

Instead we do analyses \emph{as we go}. The idea is to ask a question
and let the simulation anwer it. It we ask a different question, we
re-run the simulation, with a different Logger.

Not that the Simulation accepts exactly one Logger. If we need to log
multiple things, then we create a new Logger from several other
Loggers.

The |LoggerState| is basically just a list of log-entries of type |l|.

\begin{code}
data LoggerState l = Lgs {getLog :: [l]}
\end{code}

A Logger appends log entries to the LoggerState and returns a new
versions of itself.

\begin{code}
data Logger a l = Lgr {
  runLogger :: a -> LoggerState l -> (LoggerState l, Logger a l)
  }
\end{code}

To create a Logger we must specify \emph{when} to log and \emph{what}
to log.

\subsubsection{LogCondition}

A LogCondition answers the question \emph{when} to log. It returns a new
version of itself, which is actually the only reason why a Logger
returns a new version of itself.


\begin{code}
type LogCondition a = Mealy a Bool
\end{code}

\needspace{15em}
Here are some ready-made LogConditions

\begin{code}

-- | Create a 'logCondition' from a constant predicate. 
logIf :: (a->Bool) -> LogCondition a
logIf p = Mealy $ \a -> (p a, logIf p)
\end{code}

\begin{code}
-- | log every n invocations          
logEveryN :: Int -> Int -> LogCondition a 
logEveryN n i = Mealy f
        where
            f _ = if i >= n
                  then (True,  logEveryN n (i-n))
                  else (False, logEveryN n (i+1))

-- | Start logging at t=t0 and then every dt units of time
logEveryT :: Interval -> Instant -> LogCondition (Timed a)
logEveryT dt t0 = Mealy f
        where
            f (t,_)  = if t >= t0
                       then (True,  logEveryT dt (t0 + dt))
                       else (False, logEveryT dt t0)
\end{code}

Several LogConditions can be combined into one.

\begin{code}
logOp :: (Bool->Bool->Bool) -> LogCondition a -> LogCondition a -> LogCondition a
logOp op lgc1 lgc2 = op <$> lgc1 <*> lgc2


\end{code}

\subsubsection{LogFormatter}

A |LogFormatter| answers the question \emph{what} to log. It returns a
list of log-entries.

\begin{code}
data LogFormatter a l = Fmt {
            runFormatter :: a -> [l]
        }

\end{code}

\needspace{20em}
\subsubsection{Combinators}

\begin{code}
-- | Create a logger from 'LogCondition' and 'Formatter'
logger :: LogCondition a -> LogFormatter a l -> Logger a l
logger lgc fmt = Lgr f
        where
            f a (Lgs l) = let !(!b, !lgc') = runMealy lgc a
                              !lgr' = logger lgc' fmt
                          in if b
                             then (Lgs $ runFormatter fmt a ++ l, lgr')
                             else (Lgs l, lgr')

-- | Create a Logger from multiple existing loggers xxx ugly
loggers :: [Logger a l] -> Logger a l
loggers lgrs = Lgr run
        where
            run a log     = (log', loggers lgrs')
                    where
                        !(!log', !lgrs') = foldr f (log, []) lgrs
                        f lgr (l,g)   = let !(!l'', !lgr'') = runLogger lgr a l
                                        in  (l'', lgr'':g)



\end{code}

\begin{code}

--   runLogger :: a -> LoggerState l -> (LoggerState l, Logger a l)
xrunLogger :: (Logger a l) -> a -> Writer l (Logger a l)
xrunLogger = undefined


\end{code}
