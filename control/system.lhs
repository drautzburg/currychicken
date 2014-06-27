\documentclass[a4paper]{article}
%include lhs2TeX.fmt
%include greek.fmt
%options ghci -fglasgow-exts

\usepackage{tikz}
\usetikzlibrary{arrows,mindmap,backgrounds}
\usepackage{fancyhdr}
\usepackage{needspace}
\usepackage{amsmath}
\usepackage{graphicx}
\usepackage{capt-of}
\usepackage{ragged2e}
\usepackage[parfill]{parskip}
\usepackage{ctable}
\usepackage{color}
\usepackage{colortbl}	
\usepackage{pgf}
\usepackage[framemethod=tikz]{mdframed}
\newmdenv[frametitle=Running it,backgroundcolor=gray!05,roundcorner=2pt]{run}

\hyphenpenalty=750

\begin{document}
\newcommand{\dtzgraphic}[3]{
	\begin{minipage}{\columnwidth}\centering
          \smallskip
	\includegraphics[width=#1]{graphics/#2}
	\captionof{figure}{#3}
	\end{minipage}
}
\definecolor{shaded}{rgb}{0.9,0.9,0.9}
%\renewcommand{\arraystretch}{1.5}
\pagestyle{fancy}
\RaggedRight

\tableofcontents

\begin{code}
import qualified Data.List as L
\end{code}
%------------------------------------------------------------
\section{What the system does}
%------------------------------------------------------------

The purpose of the system is to compute present and future |System
State|s. The System State itself serves several purposes.

\begin{itemize}
\item It should allow identifying upcoming problems, such as
  \begin{itemize}
  \item Idle times (wasted resources)
  \item Resource shortage
  \item Closing time (SLA) violations
  \end{itemize}
\item It should predict the future resource demand.
\item It should allow to produce metrics which indicate ``how good we are''
\item It should indicate decisions which need to be taken by a human.
\end{itemize}

The computation shall be able to run as a \emph{Simulation},
i.e. without any input from the real world, and alternatively as
\emph{Forecast} which takes real-world observations into account.

\needspace{5em}
The input to this computation is the following
\begin{description}
\item[An initial State]
\item[Real-world observations] are generated by sensors or by humans
  in the real world. They always lie in the past and they only occur
  when making Forecasts and not during simulations.
\item[Planning data] are a set of rules which control how future
  states emerge from past States.
\item[Human decisions] override decisions the system would make purely
  on the basis of planning data and observations.
\end{description}


%------------------------------------------------------------
\section{The System State}
%------------------------------------------------------------

\subsection {Elementary Processes}

All processing is done by composing Process Chains from the following
elementary Processes:

\begin{description}
\item[Transport] changes Position and consumes Time
\item[Buffer] decouples processes
\item[Pack] puts Items into a container. Instantaneous.
\item[Unpack] removes items from a container. Instantaneous.
\item[Merge] merges several streams of items. Requires a plan to
  handle priorities. Instantaneous.
\item[Split] splits a stram of items into several strams. Requires a
  Sortplan. Instantaneous.
\item[Throttle] limits the thoughput of Items. Simplifies modelling 
  other Processes.
\end{description}

\subsection{Process State}

The six ($+1$ if you include |Throttle|) elementary Processes fall
apart into two different categories, which undergo different State
changes.

\begin{description}

\item[Pack, Unpack, Merge and Split] do not have much of an internal
  state. They don't hold items and operate instantanious. The |Merge|
  and |Split| processes however, depend on a |Program|
  (e.g. Sortplan), which may change over time

\item[Throttle] process does hold an item, but it is more of an
  internal affair to the simulation and of no interest to the user or
  planner.

\item[Buffer and Transport] are the processes which shine though to
  higher levels. Both internally hold items. We'll focus on these two.

\end{description}

\subsection {Production Order State}

A |Production Order| tells the System to ``work off'' some Material
and includes instructions how to do this. A Production Order always
begins and ends with |Transports| or |Buffers|.

A Production Order consumes Material and produces Material
elsewhere. The Material may additionally undergo some changes, such as
cancelling or coding. 

The total Material which needs to be worked off by a Production Order
is described by a number of |Batches|. 

Batches often arrive via Transports (i.e. instantaneously). But
sometimes Batches are produced gradually and they can be consumed
while they are being prouduced. Such Batches may never be fully
located in one place.

Hence a Production Order consumes and produces Batches. 

It is possible to compute to what percentage the imput Batches are
consumed. A Production order terminates when this percentage has
reached 100\%. In fact the term |Batch| only makes sense in
conjunction with Production Orders. Just by looking at your Buffers
you cannot determine this percentage.

\subsection{Resource Pool State}

As for the |ResourcePool| there are several ways of modelling
it. 

First, one might say, that it consists of a set of allocated an
unallocated |Resources|. While this sounds intuitive, it makes it
difficult to redo resource allocation (in the light of new Facts.

It is probably better to see the Resource Pool as an initial Pool plus
a number of timed allocations and deallocations. The initial Pool
needs to be known as a Fact. Now you can always replace you
allocations by different allocations and you will get a new projected
history of the Pool.

|Allocations| are acutually the consequence of |ResourceRequest|s. So
one might even see the Resource Pool as an initial Pool plut a number
of (timed) Resource Requests.



%------------------------------------------------------------
\section{Main Entities}
%------------------------------------------------------------

\subsection{Item}

An Item is either a single Container or a number of Items packed
inside a Container. With these semantics the hierarchy does not
necessarily end with Mailpieces. E.g \emph{empty Trays} are Items too,
but they are not packed, they do not contain Mailpieces and they are
leaves of this hierarchy.

Also we do not really demand, that all items inside a Container have
the same Type (i.e. Container). We can pack both Mailpieces and Trays
into a Rollcontainer.

Furthermore we do not impose any restrictions on what can be packed
inside what. We allow rediculous packings like Vehicels inside
Trays. It is up to the application to avoid such nonsense.

\begin{code}
type Id = String 
type Sortcode = String
type Label = String

data Container = Tray      Label | 
                 Rollcont  Label | 
                 Vehicle   Label | 
                 Mailpiece Id Sortcode 
                 deriving (Eq, Show)

data Item      = Single Container | Packed Container [Item] 
                 deriving(Eq,Show)
\end{code}

\begin{code}
pack :: [Item] -> Item -> Item
pack items (Single c)        = Packed c items
pack items (Packed c pitems) = Packed c (items ++ pitems)

\end{code}

\subsection{Time}

We have to deal with two kinds of time

\begin{description}

\item[Transcation Time] is the time when data enters or leaves the
  system. The transcation time can never be in the future.

\item[Valid Time] is the time of the simulated world in the system. it
  conains past, present and future moments.
\end{description}

To illustrate this take the following sentence: \emph{"yesterday I
  thought it would be raining today"}. Here |yesterday| is a
|Transaction Time|. Yesterday you told the system, that it will rain
the next day. |Today| is a |valid time|. It was in the future at
|Transaction Time| but will drift into the past.

Time comes in two flavors: an |Instant| is a point-in-time and a
|Duration| is a time delta, such that $Instant_1 + Duration =
Instant_2$ \footnote { We need to clarify whether we mean Duration or
  Interval.

  An interval in Joda-Time represents an interval of time from one
  millisecond instant to another instant. 

  A duration in Joda-Time represents a duration of time measured in
  milliseconds. 

  A period differs from a duration in that it is inexact in terms of
  milliseconds. A period can only be resolved to an exact number of
  milliseconds by specifying the instant it is relative
  to. e.g. consider the period of 1 year, if we add this to January
  1st we will always arrive at the next January 1st but the duration
  will depend on whether the intervening year is a leap year or not.
}

\begin{code}
type TrnInstant    = Integer
type ValInstant    = Integer
type ValDuration     = Integer

type Timed a = (ValInstant,a)
\end{code}

\subsection{Position}

We can say, that at a certain point in time an |Item| is at a certain
position. We discussed Time already, now how about |Position|?

When two Processes exchange Items, then the sending Process must
deliver Items at a Position, where the receiving Process can pick them
up. 

A |Trasport| receives items at one Position and moves them to another
Position. In general there will be a |Buffer| at the end of the
Transport (think: a parking space), which will store the item (the
vehicle). Hence the destination Position of the Transport must be
equal to the the Position of the Buffer.

The same considerations apply to all other Process Chains. A sorting
machine has a Feeder (a |Buffer|) which is connected to a |Split|
Process. The Buffer and the input of the Split process must be at the
same position. The Split Process has several outputs each equipped
with a Bauffer (a Stacker). The Position of a Split output must be the
same as the Position of its associated Buffer.

|Position| does not need many attributes. In fact, all we need is some
way to uniquely identify Positions. Hence we are free to chose any
data structure which includes a unique key.

\begin{code}
type Position = Integer
\end{code}

\subsection{Observations}

\begin{description}

\item[Observations] Things which have been observed are called
  |Observations| or |known Fact|s. Their |valid time| always lies in
  the past (you cannot observe the future) and their |transaction
  time| is either present or past.

\item[Estimates] Things whose |Valid Time| is in the past, but which
  were not actually observed are called |Estimates|. These are things
  you \emph{could} have observed, but did not. These are things like
  the (past) arrival of a plane, which you estimated from the
  departure time, but you never got feedback, whether or not the plane
  has actually landed.

\item[Predictions] Things whose |valid time| lies in the future are
  called |Predictions|. As time goes by, they will turn into
  |Estimates| and maybe |Facts|

\end{description}


\section{State Changes}

\subsection{The initial state}

It is interesting to ask yourself the question, what the system looks
like at the very beginning. There will be some known, estimated or
predicted workload, that we must respond to. 

This workload will be associated with just two Process Types, namely
|Transport| and |Buffer|. As long as there is no material in any of
our Buffers and we do not expect any incoming Transports, there is
nothing to do.


\subsection{Buffer and Transport States}

In the following chapters we often use primed types (|Foo'|) for values
which are not associated with Time and a corresponding primed type
(|Foo|) for |Timed| types, i.e. types which are associated with Time.

\subsubsection{Buffer States}

A Buffer is either empty, holding a number of items or is full. Unless
the Buffer is empty, it holds a number of items.

\begin{code}
data BufState' = BufEmpty | BufHolding [Item] | BufFull [Item]
type BufState  = Timed BufState'

\end{code}

There are only two Events which cause a State Change

\begin{code}
data BufferEvent' = BufIn Item | BufOut Item
type BufferEvent  = Timed BufferEvent'
\end{code}

Under which conditions a Buffer is full (or empty) depends on the size
of the buffer. We demand a buffer-specific function, which reports the
|BufState| from its items. We assume, that every Buffer can at least
hold a single item.

\needspace{10em}
\begin{code}
type BufStateFun = [Item] -> BufState'

exampleBufStateFun items = 
-- Buffer with a capacity of 10 items (of any container)
        case length items of
            0  -> BufEmpty
            10 -> BufFull    items
            _  -> BufHolding items
\end{code}

Then  the state transitions are as follows

\begin{code}

bufferStrans :: BufStateFun -> BufState -> BufferEvent -> BufState
                
bufferStrans bsf (ts,bufState) (te,event) = (te',bsf newItems)
        where
            te'      = if (te < ts) then te
                       else error "Cannot apply event to future State."
            newItems = 
                    case (bufState, event) of
                        (BufEmpty,         BufIn item)  -> [item]
                        (BufHolding items, BufIn item)  -> item : items
                        (BufFull items ,   BufOut item) -> filter (/= item) items
                        (BufHolding items, BufOut item) -> filter (/= item) items
                        (_, _)  -> error "illegal buffer operation"

\end{code}

\subsubsection{Transport States}


A Transport is either started or stopped. A Transport always holds an
Item (often a |Vehicle|). A started Transport additionally hold the
|estimated time until arrival|.

\begin{code}
type Eta = ValDuration
data TrpState' = TrpStopped Item | TrpStarted Item Eta
type TrpState  = Timed TrpState'
\end{code}

There are three Events which cause a State Change

\begin{code}

data TrpEvent' = TrpDepart Item | TrpArrive | TrpEnRoute Position
type TrpEvent  = Timed TrpEvent'
\end{code}

We demand a transport-specific function, which computes the estimated
time \emph{until} arrival (|Eta|) whenever a Transport was seen.

\begin{code}
type TrpEtaFun = TrpEvent' -> Eta

exampleTrpEtaFun :: TrpEtaFun
exampleTrpEtaFun event = 
        case event of
            TrpDepart _          -> 100
            -- 18123 is some position in the middle of the Transport
            TrpEnRoute 18123     -> 50
            TrpArrive            -> 0
\end{code}

Then  the state transitions are as follows

\begin{code}
transportStrans :: TrpEtaFun -> TrpState -> TrpEvent -> TrpState
transportStrans tef (ts,trpState) (te,event) = (te', newState)
        where
            te'      = if (te < ts) then te
                       else error "Cannot apply event to future State."
            eta      = tef event
            newState = 
                    case (trpState, event) of
                        (TrpStopped _,      TrpDepart item)  -> TrpStarted item eta
                        (TrpStarted item _, TrpEnRoute pos)  -> TrpStarted item eta
                        (TrpStarted item _, TrpArrive)       -> TrpStopped item 
                    

\end{code}

\section{Process Chain}

If there is something to do, we need to set up a process chain which
handles the workload.  Setting up a process chain includes requesting
|Resources| and the resulting Process chain may depend on the
availability of Resources.

There are several things we demand from a Process Chain

\begin{itemize}
  \item A Process chain should always start and end with |Buffers| or
    |Transports|.
  \item A Process Chain must have identifiable inputs and outputs.
  \item A Process Chain maintains an internal |State|. The internal
    State is the union of the States of the elementary Proceses it is
    built from.
  \item A Process Chain responds to |Event|s. Event can be external,
    i.e. Events emitted by its bordering Buffers and Transports, or
    they can be internal events, which nobody cares about, except the
    Process Chain itself.
  \item A Process Chain should provide a minimal control interface
    which allows starting and stopping it or to change Sortplan (in
    case of a Sorting Run)
  \item a Process Chain can emit events. Again there are extenal
    events, which are sent to its bordering Buffers and Transports and
    internal Events.
\end{itemize}

\subsection{Using planned Process Chains}

In our Opal discussions, we set up (much of) a Process Chain at
\emph{planning} time. This is indeed one way of doing it, but
certainly not the only way. 

Such a planned process chain can be paraphrased something like ``At
21:00 we run Sortplan 100 on IRV1, IRV2 and IRV3''. In this case the
triggering Event, which instantiates this Process Chain it the (valid
time) clock reaching 21:00.

But what if there is no Material to process at 21:00?  And what if
IRV2 is not available at this time? So we need to inform the user if
any of these two things happen, because the system itself has no means
to resolve the issue.

Having no material to process is already bad enough. But we might
instantiate the Process Chain nonetheless. This will waste some
resources, but it will not be a logical contradiction.

Using a Resource which isn't there is more severe. Whatever
conclusions the system will draw from this setup will be outright
\emph{wrong}. One thing we can do here is to use a more intelligent
|Resource Allocation| method, e.g

\begin{itemize}
\item use only available Resources. If requested Resources aren't
  availble, this may slow down the process or bring it to a halt.
\item specify Resource Requests in a more flexible way. We could
  distinguish between |named| Resources (IRV1...) and |numbered|
  Resouces (Machines capable or running Plan 100). This will make the
  \emph{Resource unavailable} Situation less likely, but it can still
  happen.
\item prioritize Resource requests. It makes little sense to allocate
  Resources in the order of the transaction times of Resource Requests
  (first-come-first-serve). We should be able to undo allocations when
  a Resource is needed for a high-priority task. Again this will not
  eliminate \emph{Resource unavailable} Situations, but will make them
  less likely to occur and less likely to affect important tasks.
\end{itemize}

If we look at this Process Chain in more detail, we will find, that
the chain begins at some |Buffer| where the incoming material gets
stored. There will be a |Split| Process which splits up the material
for IRV1, IRV2 and IRV3. There will be three |Transports| which bring
the split material to the Feeders of the three machines. In fact there
will be 3xn such Transports, because the Material will not be moved in
one big chunk. Then we have three |Unpack| each associated with a
|Split| Process which stand for the actual sorting. The Split
Processes end up in |Buffers| (the Stackers). Each Stacker has a
|Pack| Process which puts the Material into Trays. Those Trays are
move to a |Merge| Process by means of many more |Transport|s. The
Merge Process finally deposits the Trays in yet another |Buffer|.

This entire network of interconnected Processes is pre-fabricated and
ready to be instantiated. And it indeed has Buffers at both its inputs
and its outputs.

If IRV2 is not available, we must erase part of this Chain. The
|Split| process will lose one output, the |Transport|s departing at
this output will go . The IRV2 |Split| Process will go and so will its
associated |Pack| and |Transport| Processes. Finally the |Merge|
Process will lose one input. Once all this is done, we're again in
line with reality and the Process Chain will produce \emph{correct}
(albeit possibly undesirable) results.


Asking the user to respond to the \emph{Resource unavailable}
situation means asking him to do all of the above. Of course, the
system may offer great support to eliminate all the tedious work, but
this is what it boils down to.



\end{document}