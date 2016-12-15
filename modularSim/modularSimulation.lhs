\documentclass{article}
%include lhs2TeX.fmt
%include lhs2TeX.sty

\usepackage{graphicx}
\usepackage{float}
\usepackage[parfill]{parskip}
\usepackage{needspace}

%options ghci
\usepackage[framemethod=tikz]{mdframed}
\newmdenv[frametitle=Running it,backgroundcolor=gray!05,roundcorner=2pt]{run}
%\usepackage{ucs} 
%\usepackage[utf8]{inputenc}

\usepackage{amssymb,amsmath}
%include lhs2TeX.fmt
%include lhs2TeX.sty

\widowpenalty=3000
\clubpenalty=3000

\author{Martin Drautzburg}

\title{Modular Simulation}

\begin{document} \maketitle \tableofcontents 

\begin{abstract} 
The main purpose of a simulation is to compute future States from a
known current State. Hence you would expect to find functions from
State to State. 

However, Simulations tend to be centered around Events. As it turns
out, Events are not essential for a Simulation, but rather an act of
optimization. 

Once the need (or lack thereof) for Events is understood, a natural
solution appears, which allows building large simulations from small
pieces.
\end{abstract}

% ------------------------------------------------------------
\section{Introduction}

\subsection{Steppers}

\begin{figure}[H]
  \centering
    \includegraphics[width=2cm]{modularSimulation0.eps}
  \caption{Steppers compute future States}
\end{figure}


Computing the next System State can be a complex thing to do. Hence it
is very desirable to split this task into a set of smaller tasks
(Steppers). Each Stepper takes a System State and answers along the
lines of \emph{``If I had a say in this, then the next System State would be
this''}.

A System State always includes a \emph{time} aspect. If we have
multiples Steppers, then each of them may answer a System State with
a different time attached to it. Only the \emph{nearest} projected
System State is relevant. Someone needs to supervise all the Steppers
and choose the nearest future System State.


\begin{figure}[H]
  \centering
    \includegraphics[width=5cm]{modularSimulation1.eps}
  \caption{Combining eventless Steppers}
\end{figure}

The combined system of Steppers and a Supervisor, is a Stepper
itself. After all it computes a new System State from a known System
State. This suggests that you can stack up Steppers in a hierarchical
way, where the top-level Stepper represents the Simulation as a
whole. With this way of thinking, Events are not required.


\subsection{Events}

\begin{figure}[H]
  \centering
    \includegraphics[width=2.5cm]{modularSimulation2.eps}
  \caption{Stepper with Events}
  \label{fig:stepperOp}
\end{figure}

Events are somewhat ugly. In simulation tutorials you often read, that
events are placed into an Event-queue and then this queue is worked
off in a chronological fashion. Hovever, some Events in the Queue may
get obsoleted by other preceding Events. One needs to decide which
Events can stay and which need to go. This may easily require
superhuman foresight and skills.

Still the event-less Stepper-only approach has a significant drawback:
many potential System States get computed, but only one of them will
eventually make it. 

\subsection{Combining Steppers with shared State}

To select the next System State, only the \emph{time} of the
State-change needs to be considered. So it would make some sense, to
have each Stepper compute the time of its next State-change first, and
only when a supervisor selects this Stepper have it compute the next
System State.


\begin{figure}[H]
  \centering
    \includegraphics[width=5cm]{modularSimulation3.eps}
  \caption{Combining eventful Steppers}
\end{figure}

This particularly makes sense, when it is much cheaper to compute the
next Event than to compute the next SystemState. 

With this setup, the Steppers, whose computed future States were
discarded, don't have to do anything, because the new System State is
fully computed by the winning Stepper.

Event have a very short lifespan in this setup. They are only needed
until a new SystemState has been computed. If you consider a
simulation with an event-queue, then this is equivalent to emptying
the queue completely once a new SystemState is known. Hence you don't
need any superhuman foresight to decide which events to keep, because
\emph{all} Events will be discarded.

\subsection{Combining Steppers with split State}

Different Steppers can operate on different aspects of the System
State. This is particularly useful, when you simulate communicating
processes, where each process has its own simulation and the processes
do not share State (think: Opal Control vs. simWorld).

\begin{figure}[H]
  \centering
    \includegraphics[width=5cm]{modularSimulation4.eps}
  \caption{Split System State}
\end{figure}


With this setup, the Steppers, whose computed future States were
discarded (acutually their Events), will have to return their old
System States more or less unchanged. Only the time needs to get
advanced. They can no longer assume that some winning Stepper will do
that.  The supervisor (|earliest|) will have to call each Stepper's
|nxtState| function and send the losers some dummy event, indicating,
they shoud just advance the time.


\section{Implementation}

\begin{code}
import Data.Either
import Data.Maybe
import Data.Monoid
import Data.IntSet
import Control.DeepSeq
import Control.Concurrent
import Debug.Trace
import System.TimeIt

type Time       = Int
data SysState s = SysState Time s deriving (Show)
stateTime (SysState t _) = t
stateData (SysState _ d) = d


data Event e    = Event Time e
evtTime (Event t _) = t
evtData (Event _ d) = d
\end{code}

\subsection{Running a Simulation}

The main operation to run a Stepper follows directly from
Figure~\ref{fig:stepperOp}

\begin{itemize}
\item Ask a stepper for the next event by calling its |nxtEvt|
  function, passing it the current SystemState
\item Call the same Stepper's |nxtState| function passing the same
  SystemState and the Event we just obtained. This will give us a new
  SystemState.
\item Continue with the new SystemState or end the simulation
\end{itemize}

\subsubsection{Logging}

We are almost never interested in the whole set of SystemStates. We
really want to create a log of "interesting" observations, a
\emph{KPI-log}. It is best to produce the KPIs as we go along. This
way many of the intermediate SystemStates can be garbage collected
right away, reducing the memory footprint significantly.

What could such a log-creating function look like? No doubt it has to
look at the current SystemState and the log produced so far (e.g. to
append to it).

But a Logger may also want to have its own internal state. Then it
could e.g. remember previous values (states or intermediate values)
and refer to them later. But this does not state an extra requirement:
a logger can store anything it likes in its log, including a running
state.

Hence a logger has the following type:

\begin{code}
type Logger s l = SysState s -> l -> l
\end{code}

\subsubsection{Termination}

Another question is when the simulation shall terminate. There are
three conditions under which the simulation will terminate:

\begin{itemize}
\item If two successive SystemStates are the same, then the State will
  remain constant for all time and we can terminate the simulation.
\item If there is no next Event, the SystemState will not change
  either.
\item We can set a termination condition ourselves by e.g. setting an
  upper limit for the Time of the simulation.
\end{itemize}

For the latter we define another function, which looks at the
SystemState and returns |True| when done, i.e. whose type is

\begin{code}
type Terminator s = SysState s -> Bool
\end{code}

\subsubsection{RunContext}

\needspace{4em}
These two functions together make a \emph{RunContext}:
\begin{code}
data RunContext s l = RunContext {
    logger     :: Logger s l ,
    terminator :: Terminator s
}
\end{code}

Note that the |RunContext| is independant of the Steppers and only
depends on the type of the SystemState and the type of log we want to
produce.

\subsubsection{runStepper}

Finally we must make sure the SystemState is fully evaluated at each
recursion (|deepseq|), otherwise thunks will pile up on the stack
leading to a stack overflow once we make a million iterations. You
won't have \emph{this} problem in an imperative language.

\needspace{20em}
Putting it all together gives us:

\begin{code}
runStepper :: NFData s => (Stepper s e) -> RunContext s l -> l -> SysState s 
           -> (SysState s, l)
runStepper stp rc log state = 
        let
                evt    = nxtEvt stp state
                state' = case evt of
                             Nothing -> state
                             Just e  -> nxtState stp state e
                log'   = logger rc state' log
        in
            if (terminator rc state') || isNothing evt
            then (state', log')
            else (stateTime state', stateData state') 
                         `deepseq` 
                         runStepper stp rc log' state'
\end{code}

\subsubsection{Shorthands}

To define a Stepper which does things at certain times, requires some
thought. After all it only sees the SystemState, which \emph{is}
associated with a timestamp, but it cannot just add a fixed time-delta
to produce its next Event. If we want to fire an Event at fixed
intervals, we have to do something like:

\begin{code}
cronNxtEvt :: Time -> e -> NxtEvt s e
cronNxtEvt dt eventData (SysState t s) = Just $ Event t' eventData
        where
            t' = dt * (t `div` dt) + dt
\end{code}

And while we're at it, let's define some shorthand |NxtState|
function, which performs an operation on the state whenever it
receives an Event.

\needspace{4em}
\begin{code}
opNxtState :: (s -> s) -> NxtState s e
opNxtState op = nxtState
        where
            nxtState sState (Event t _) = SysState t (op $ stateData sState)


\end{code}


\subsubsection{Example}

Let's define a very simple Stepper. The SystemState shall be a mere
integer, which is zero at $t=0$. The Event type shall be |()| (think:
void). An Event shall be fired every 3 units of time and the state
shall increment by one.


\needspace{4em}
So this is our example Stepper:
\begin{code}
exStepper1 :: Stepper Int ()
exStepper1 = Stepper (cronNxtEvt 3 ()) (opNxtState (+1))
\end{code}

Finally we need to create a |RunContext|.
\begin{code}
exRunContext :: RunContext Int [String]
exRunContext = RunContext myLogger myTerm
        where
            myLogger (SysState t s) [] = ["not logging"]
            myLogger (SysState t s) l  = l
            myTerm   (SysState t s)   = s >= 100000

\end{code}

Our initial log shall be |[]| and the initial State shall be this:

\begin{code}
exState1 = SysState 0 0 :: SysState Int
\end{code}

To test the execution time we use
\begin{code}
bench = timeIt . putStrLn . show 
\end{code}
\needspace{4em}
\begin{run}
\needspace{4em}
|*Main> bench $ runStepper exStepper1 exRunContext [] exState1|\\
  \eval{bench $ runStepper exStepper1 exRunContext [] exState1}
\end{run}

This is interpreted code. For compiled code we get
\begin{verbatim}
SysState 300000.0 100000
CPU time:   0.05s
\end{verbatim}

This is the overhead of the simulation as such, as the computations
from State to State are very simple. We need roughly $\frac{1}{2}sec$
for a million iterations.

\subsection{Shared State implementation}

A |Stepper| primarily consists of two functions, one to compute the
next Event and another to compute the next State.

\begin{code}

-- the types of the two functions
type NxtEvt   s e = SysState s -> Maybe (Event e)
type NxtState s e = SysState s -> Event e -> SysState s

-- combining them makes a Stepper
data Stepper s e = Stepper {
            nxtEvt   :: NxtEvt   s e,
            nxtState :: NxtState s e
}
\end{code}

We can create a new |Stepper| by combining two Steppers, which operate
on the same State. To do so, we will have to compare the Events
returned by each of the Steppers, such that the Event which the
earliest time wins. We must also consider the situation, where a
Stepper returns no Event at all. If both steppers return no Event,
then the comparison shall also return Nothing.

\begin{code}

cmpEvts :: Maybe (Event e1) -> Maybe (Event e2) -> Maybe (Time, Ordering)
cmpEvts e1 e2 = 
        case (e1,e2) of
            (Nothing, Nothing)            -> Nothing
            (Nothing, Just (Event t _))   -> Just (t, GT)
            (Just (Event t _), Nothing)   -> Just (t, LT)

            (Just (Event t1 _), 
             Just (Event t2 _))           -> Just (
                                                   min t1 t2, 
                                                   compare t1 t2
                                                  )
                                                  
\end{code}


To define a combined Stepper, we must define the two functions
|nxtEvt| and |nxtState|. |nxtEvt| must return a (combined) Event,
whose data type we're free to choose, as long as |nxtState| understands
it. We chose to remember as the new Event data :

\begin{itemize}
\item the |nxtState| function of the chosen Stepper along with
\item the Event the Stepper reported. 
\end{itemize}

The type of the SystemState is easier: it is the same for each Stepper
and the combined Stepper.

This is all we need to know in order to define a |nxtState|
function. Because the Event types of the two Steppers may differ, we
must wrap the result in an |Either| type (think: union).

\begin{code}
mkStepper :: Stepper s e1 -> Stepper s e2 
        -> Stepper s (Either e1 e2)
\end{code}


\needspace{18em}
Now we implement the two functions:
\begin{code}
mkStepper stp1 stp2 = 
        Stepper myNxtEvt myNxtState
        where
            myNxtEvt sState = 
                    let 
                            evtLeft      = nxtEvt stp1 sState
                            evtRight     = nxtEvt stp2 sState
                    in 
                        case cmpEvts evtLeft evtRight of
                            Nothing      -> Nothing
                            Just (t, LT) -> mkLrEvt Left  t evtLeft
                            Just (t, GT) -> mkLrEvt Right t evtRight
                            Just (t, EQ) -> mkLrEvt Right t evtRight
            myNxtState sState evt =
                    case evt of
                        Event t (Left e) 
                                -> nxtState stp1 sState (Event t e)
                        Event t (Right e) 
                                -> nxtState stp2 sState (Event t e)
            -- shorthands:
            getJustDat   = evtData . fromJust 
            mkLrEvt lr t = Just . (Event t) . lr . getJustDat 

\end{code}


\begin{code}

\end{code}


\begin{code}
main = bench $ runStepper exStepper1 exRunContext [] exState1
\end{code}



\begin{code}
-- runWorldStepper :: NFData s => (Stepper s e) -> RunContext s l -> l -> SysState s 
--           -> (SysState s, l)
runWorldStepper stp rc log state = 
        let
                evt    = nxtEvt stp state
                state' = case evt of
                             Nothing -> state
                             Just e  -> nxtState stp state e
                evt'   = getChar
                log'   = logger rc state' log
        in
            if (terminator rc state') || isNothing evt
            then (state', log')
            else (stateTime state', stateData state') 
                         `deepseq` 
                         runStepper stp rc log' state'
\end{code}


\end{document}


