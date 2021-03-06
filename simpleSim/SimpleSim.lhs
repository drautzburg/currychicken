%let source=True
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
\usepackage{enumitem}
\usepackage[framemethod=tikz]{mdframed}
\newmdenv[frametitle=Running it,backgroundcolor=gray!05,roundcorner=2pt]{run}
\renewcommand{\texfamily}{\fontfamily{cmtex}\selectfont\small~~~~~}
%\RaggedRight
% --------------
% tikz
% --------------
%\usepackage{tikz}
%\usetikzlibrary{calc} 
%\usetikzlibrary{decorations}
%\usetikzlibrary{plotmarks}
%\usetikzlibrary{arrows}
%\usetikzlibrary{chains,fit,shapes}

\usepackage{hyperref}
\hypersetup{colorlinks=true, linkcolor=blue, pdftoolbar=true}


% ------------------------------------------------------------
\author{Martin Drautzburg}
\title{Beggar's Simulation}
\begin{document} \maketitle 

\begin{abstract}
  I'll present a simple method to predict loads and indirectly also
  resource-requirements in a hub, which is so simple that calling it a
  ``Poor man's Simulation`' would already be an overstatement.

  This document is sprinkled with haskell code, which is an excellent
  way to verify your ideas are sound (think: executable specification).
%if source
%else
Most of the time the implementation is hidden and only types (think:
function prototypes) are shown.
%endif
\end{abstract}


\tableofcontents 
\listoffigures

\pagebreak
\section{Introduction}

The following ideas are centered around the terms \verb!Classification!,
\verb!Material!,
\verb!Batch!,
\verb!Segment! and
\verb!Stream!. These entities are related as follows:

\begin{figure}[htb!]
\centering
\caption{Illustrative ERD}
\label{fig:erd}
\includegraphics[width=12cm]{simpleSimERD.png}
\end{figure}


\section{Material}

Material is described by a number of classification \verb!c! and the
number of items which fall into each classification. Furthermore
Material can contain other Material, its \verb!subs!.

%if False
\begin{code}
module SimpleSim where
import Data.List
import Data.List.Split
import Data.Function
import Text.Printf
import Text.PrettyPrint
import Text.PrettyPrint.GenericPretty
import System.Random
import Debug.Trace
\end{code}
%endif

\begin{code}
data Material c = M [(c, Count, Material c)] | NoMaterial
              deriving (Eq, Show)

type Count = Double

-- A clafficiation suitable for SF-Express
type SfClassification = (Class, Format, Destination)

data Class       = Speed|Std
                 deriving (Eq, Ord, Show)
data Format      = Flyer | Parcel | Bag
                 deriving (Eq, Ord, Show)
data Destination = To String
                 deriving (Eq, Ord, Show)
\end{code}


\subsubsection{Accessing}

We need some functions to easily access the components of
\verb!Material! and a function to get the total volume.

\begin{code}
classification (cl, n, subs) = cl
count          (cl, n, subs) = n
subs           (cl, n, subs) = maScale n subs

-- total volume of material
maCount :: (Material c) -> Count
\end{code}

%if source
\begin{code}
maCount NoMaterial = 0
maCount (M m) = sum ( map count  m)

maScale k (M m) = M (map mul m)
  where
    mul (c, n, subs) = (c, k*n, subs)
\end{code}
%endif

Material can be merged with other Material or a whole set of Materials
can be merged into a single one. 

\begin{code}
-- merge two
maMerge :: (Ord c) => (Material c) -> (Material c)-> (Material c)

-- merge any number
maMergeAll :: (Ord c) => [(Material c)] -> (Material c)
\end{code}

%if source
\begin{code}
maMerge m NoMaterial  = m
maMerge NoMaterial m  = m
maMerge (M m1) (M m2) = M (m1 ++ m2)

maMergeAll m  = foldr maMerge NoMaterial m
\end{code}
%endif

We can filter Material with a \verb!Filter! predicate.
\begin{code}
type Filter c = c -> Bool
maFilter :: (Filter c) -> (Material c) -> (Material c)
\end{code}

%if source
\begin{code}
maFilter f NoMaterial = NoMaterial
maFilter f (M m) = M $ filter (f.classification) m
\end{code}
%endif

Examples

\begin{code}
exMaterial1 = M [
  ((Speed, Flyer,  To "TXL"), 4, NoMaterial),
  ((Std,   Parcel, To "BKK"), 6, NoMaterial)] 
exMaterial2 = M [((Speed, Flyer, To "TXL"), 10, NoMaterial)] 
\end{code}

|*Main> maCount exMaterial1|\\
  \eval{maCount exMaterial1}

|*Main> maMerge exMaterial1 exMaterial2|\\
  \eval{maMerge exMaterial1 exMaterial2}

|*Main> maFilter (\(c,f,d) -> f==Flyer) (maMerge exMaterial1 exMaterial2)|\\
  \eval{maFilter (\(c,f,d) -> f==Flyer) (maMerge exMaterial1 exMaterial2)}


\section{Batches and Streams}

Material comes in two flavors. It either sits motionless in which case
we call it a \verb!Batch!, or it is flowing in which case we call it a
\verb!Stream!.

\subsection{Batches}

A Batch is described by \verb!Material! and a unique name.

\begin{code}
type Batch c = (String, Material c)
\end{code}


\begin{code}
baCount :: Batch c -> Count
\end{code}

%if source
\begin{code}
baCount (n,m) = maCount m
\end{code}
%endif

We can filter a \verb!Batch!, i.e. select items from it which fall
into a given classification by filtering the Material. This is
basically the same operation as filtering \verb!Material!.

\begin{code}
baFilter :: Filter c -> Batch c -> Batch c
\end{code}

%if source
\begin{code}
baFilter f (n, m)  = (n, maFilter f m)
\end{code}
%endif

\subsection{Streams}

Streams are expressed as how much of a given (input) Batch flows
through a measuring point. In simple cases it is sufficient to
consider a build-up, described by a line segment having a start-time,
and end-time and a throughput. However, due to merging processes there
may be several such line-segments, so we need to consider lists of
line-segments.

Note that here \verb!Throughput! means the \emph{required} throughput
to process the stream without slowing it down. It is the rate at which
material arrives. We might have called it \verb!flow rate! or
\verb!current!. There is also the throughput of a process, which
\emph{limits} the flow rate. The two concepts are very similar and
have the same units, but are not quite identical.

\begin{code}
type Time = Double
type Interval = (Time, Time)
type Segment c = (Batch c, Interval, Throughput)
type Stream c = [Segment c]
type Throughput = Double
\end{code}

\subsubsection{Accessing}

Here are some quite boring functions to access the fields of a Segment.
\begin{code}
-- getters
sgStartTime (b, (t1, t2), tp) = t1
sgEndTime   (b, (t1, t2), tp) = t2
sgInterval  (b, (t1, t2), tp) = (t1,t2)
sgTp        (b, (t1, t2), tp) = tp 

-- setters
sgSetStartTime t1' (b, (t1, t2), tp) = (b,(t1',t2 ), tp)
sgSetEndTime   t2' (b, (t1, t2), tp) = (b,(t1, t2'), tp)
sgSetInterval  (t1',t2') (b, (t1, t2), tp) = (b,(t1', t2'), tp)
sgSetTp        tp' (b, (t1, t2), tp) = (b,(t1, t2 ), tp')
\end{code}

Slightly more interesting are these operations. To scale the throughput
of a Segment. We will preserve the volume and hence need to postpone
the end time $t2$ when the throughput gets reduced.

\begin{code}
sgDelay        dt  (b, (t1, t2), tp) = (b, (t1+dt, t2+dt), tp)
sgScaleTp      k   (b, (t1, t2), tp) = (b, (t1, t1 + k*(t2-t1)), tp/k)
\end{code}

\subsubsection{Splitting}
\needspace{2em} We can split a Segment, or a List of Segments at a
point in time
\begin{code}
seSplit    :: Time -> Segment c -> [Segment c]
seSplitAll :: Time -> [Segment c] -> [Segment c]
\end{code}

%if source
\begin{code}
seSplit t segment@(b, (t1,t2), tp) 
  | t <= t1 = [segment]
  | t >= t2 = [segment]
  | otherwise = [(b,(t1,t), tp), (b,(t,t2), tp)]


seSplitAll t sgs = concatMap (seSplit t) sgs
\end{code}
%endif

\subsubsection{Creating}

We can create a Stream from a Batch if we know the throughput of the
unbuffering process and its scheduled start and end time.

Unbuffering is pretty much like unpacking, except we ignore the fact
that items are in a container. In addition to what \verb!unbuffer!
does, \verb!unpack! would reduce a level of nesting and ideally
produce a Stream of empty containers.

We assume that the batch to unbuffer is available when unbuffering
starts, but we adjust the end-time to be either the scheduled end-time
of the unbuffer process or the time when the material is exhausted -
whatever comes first.

\begin{code}
unbuffer :: Batch c -> Interval -> Throughput -> Stream c
\end{code}

%if source
\begin{code}
unbuffer batch (t1, t2) tp = [(batch, (t1, t2'), tp)]
  where
    t2'   = t1 + min (count / tp) t2
    count = baCount batch
\end{code}
%endif

\subsubsection{Accessing}

The total number of items which have passed a measuring point up to a
given time can be computed as a superposition of several ramp
curves. Likewise the total required throughput can be computed by
adding up the throughputs of all Segments.

\begin{code}
stCount      :: Stream c -> Time -> Count
stThroughput :: Stream c -> Time -> Throughput
\end{code}

%if source
\begin{code}
stCount sgs t = foldr count 0 sgs
  where
    count (b,(t1,t2),tp) n
      | t < t1    = n
      | t > t2    = n + (t2 - t1)*tp 
      | otherwise = n + (t - t1) * tp

-- total required throughput
stThroughput sgs t = sum $ map getTp sgs
  where
    getTp (b,(t1,t2),tp)
      | t < t1    = 0
      | t >= t2   = 0
      | otherwise = tp
\end{code}
%endif

%if False
\begin{code}
stPrint :: Stream c -> IO()
stPrint [] = putStrLn ""
stPrint ((b,(t1,t2),tp):sgs) = do
  putStrLn $ printf "%02.2f-%02.2f: %02.2f" t1 t2 tp 
  stPrint sgs
\end{code}
%endif

\subsubsection{Sorting}

We can sort the Segments in a Stream by their start times
\begin{code}
stSortByStart :: Stream c -> Stream c
\end{code}

%if source
\begin{code}
stSortByStart= sortBy (compare `on` sgStartTime) 
\end{code}
%endif

\subsubsection{Transformations}

We can merge two or any number of Streams and we get a new
Stream. This operation is simply a concatenation of the lists of
Segments,

\begin{code}
stMerge    :: Stream c -> Stream c -> Stream c
stMergeAll :: [Stream c] -> Stream c
\end{code}

%if source
\begin{code}
stMerge s1 s2 = s1 ++ s2
stMergeAll st  = foldr stMerge [] st
\end{code}
%endif

\needspace{4em}

Most of the Segment function can eaily be implemented as Stream
functions, simply by applying them to all Segments in the Stream.

\begin{code}
stSetStartTime:: Time       -> Stream c -> Stream c
stSetEndTime  :: Time       -> Stream c -> Stream c
stSetInterval :: Interval   -> Stream c -> Stream c
stSetTp       :: Throughput -> Stream c -> Stream c
stDelay       :: Time       -> Stream c -> Stream c
stScaleTp     :: Double     -> Stream c -> Stream c

\end{code}

%if source
\begin{code}
stDelay dt         = map (sgDelay dt)
stSetStartTime t1' = map (sgSetStartTime t1')
stSetEndTime t2'   = map (sgSetStartTime t2')
stSetInterval i    = map (sgSetInterval i)
stSetTp tp         = map (sgSetTp tp)
stScaleTp k        = map (sgScaleTp k)
\end{code}
%endif

We can filter a Stream, which alters the underlying batches and also
scales down the throughputs.

\begin{code}
stFilter :: Filter c -> Stream c -> Stream c
\end{code}

%if source
\begin{code}
stFilter f s = map f' s
  where
    f' (b,i,tp) =
      let
        b'  = baFilter f b
        tp' = tp * (baCount b') / (baCount b)
      in
       (b',i,tp')

\end{code}
%endif

\subsubsection{Packing and unpacking}

When we receive a Stream of Bags which contain Flyers, we can unpack
it and we get a Stream of Flyers. This operation removes one level of
nesting, leaves the intervals intact and adjusts the throughput. It
typically becomes larger, because now we're counting Flyers instead of
counting Bags of Flyers.

Unpacking can be defined on \verb!Material!,
\verb!Batch!,\verb!Segment! and \verb!Stream!, where the latter is
what we're really interested n.

\begin{code}
maUnpack :: Ord c => Material c -> Material c
baUnpack :: Ord c => Batch c    -> Batch c
seUnpack :: Ord c => Segment c  -> Segment c
stUnpack :: Ord c => Stream c   -> Stream c
\end{code}


%if source
\begin{code}
maUnpack (M ms) = maMergeAll $ map subs ms
baUnpack (name, m) = (name, maUnpack m)

seUnpack (b, i, tp) = (b', i, tp')
  where
    b'  = baUnpack b
    tp' = tp * baCount b' / baCount b

stUnpack st = map seUnpack st
\end{code}
%endif

For packing we need a way to associate classifications with the
created containers. Also we need to know into how many containers we
want to pack, or alternatively their sizes. I.e. we need to add the
infomation which \verb!unpack! destroyed.

\begin{code}
-- pack into n containers
pack :: c -> Count -> Material c -> (c, Count, Material c)

-- pack into s-sized containers
packSized :: c -> Count -> Material c -> (c, Count, Material c)
\end{code}

%if source
\begin{code}
pack cl n m =  (cl, n, maScale (1/n) m)

packSized cl s m = pack cl n m
  where
    n = (maCount m)/s
\end{code}
%endif

There is no exact inverse of the other unpack operations. This is
because unpack can work with a number of different containers. For the
inverse operation, we would need to know which item goes into which
container. However, this goes beyond mere packing and involves a
split. To pack a batch, we need to provide the new batch name

\marginpar{\raggedright A few things are strange here and need more thinking}
\begin{code}
maPack :: c -> Count -> Material c -> Material c
baPack :: c -> Count -> String -> Material c -> Batch c
\end{code}


%if source
\begin{code}
maPack cl s m = M [packSized cl s m]
baPack cl s name m = (name, maPack cl s m)
\end{code}
%endif

\subsubsection{Example}
We can now create an example Stream. Figure~\ref{fig:summing} shows it
individual Segments and the sum over all Segments. The throughput is
scaled up by a factor of 10, the peak throughput is actually
10.

\begin{code}
exStream1 :: Stream SfClassification
exStream1 = concat [
  unbuffer ("b" ++ show i,exMaterial1) (i, i+10) 1
  | i<- map fromIntegral [0..9]
  ] 

exStream2 :: Stream SfClassification
exStream2 = concat [
  unbuffer ("b" ++ show i,exMaterial2) (i, i+10) 1
  | i<- map fromIntegral [0..1]
  ] 

\end{code}

\begin{figure}[htb!]
\centering
\caption{Segments, Throughput  and Volume of a Stream}
\label{fig:summing}
\includegraphics[width=10cm]{summing.png}

\tiny (throughput scaled by 10)
\end{figure}


\subsubsection{Throttle}

The most difficult operation is that of a \verb!Throttle!, which
restricts the throughput of a stream. In the real world a throttle
would actually slow down its \emph{input} and have a backwater effect,
or alternatively it would have to provide a buffer where material
could pile up. But here will will simplify things considerably.

There are two ways in which the system could handle the situation
where the required throughput exceeds the available throughput.

Either it simply reports the situation, but doesn't alter the Stream
in any way. There would be no effect on downstream processes. The nice
thing is that this is easy to implement and shows problems where they
can be fixed. The bad thing is that it simply doesn't show what would
happen in the real world.

Alternatively we could alter the Stream such that the maximum
thgoughput is never exceeded. This would push the end-times to later
points in time. This is the algorithm we will implement.

Note that a similar question exists in the realm of resource
planning. You may either ask how many resources you need, or you may
ask what will happen, given the resources you have. These are two very
different questions.

We will implement \verb!throttle! by creating groups of Streams which have
matching start and end times. We will have to compute new (smaller)
throughputs for each Stream in the group and compute new (larger) end
times.

\begin{figure}[htb!]
\centering
\caption{Computing Throttle}
\label{fig:throttle}
\includegraphics[width=8cm]{throttle.png}
\end{figure}

If we consider a Stream where all Seqments are aligned, we can treat
it like a single Segment. In Figure (\ref {fig:throttle}), the black
line represents the build-up of material and the dashed blue and green
lines represent break-downs with different throughputs $tpMax$. The
build-up comes with a required throughput $tpReq$, i.e. the throughput
at which the material comes in. 

There is a distinct point in time $t_0$. If we start the breakdown
there, then we will finish when the input Stream stops. 

To compute $t_0$ we reason as follows. The total amount of Material
is $M = (t_2 - t_1) tpReq$. To process this Material at $tpMax$ we
need a duration of $\Delta t = M/tpMax$. To obtain $t_0$ we must
subtract this duration from $t_2$.

\begin{eqnarray}
t_0  &=& t_2 - (t_2 - t_1)\frac{tpReq}{tpMax}\notag \\
\notag\\
k    &=& \frac{tpReq}{tpMax}\\
\notag\\
t_0  &=& t_2 - k(t_2 - t_1)
\end{eqnarray}

With $k=1$ we can process Material as fast as it comes in. In this
case $t_0 = t_1$. With $k>1$ Material comes in faster than we can
process it. Since $t_2 - t_1 > 0$ we subtract more from $t_2$ and
$t_0$ becomes smaller than $t_1$. In this case we don't care much
about $t_0$ because we can only start processing at $t_1$ and not
earlier.

With $k<1$ we can process faster than Material comes in. This is the
interesting case. Here $t_0 >t_1$ and as $k$ approaches $0$, $t_0$
approaches $t_2$.

Throughout the computation we will maintain a time $t_x$, which is the
time up to which the output is computed. Initially $t_x$ will be $-
\infty$. Depending on $t_0$ and $t_x$ we find three different
situations.

\begin{description}
\item[$t_x> t_0$]\hfill\\ If $t_x> t_0$, then all Material is ``past''
  buffered Material. We can process it as fast as we can and it does
  not matter how fast it comes (or came) in.

\begin{eqnarray}
tp'   &=& tpMax \\
\notag\\
t_x'  &=& t_x + \frac{(t_2-t_1) tpReq}{tp'}
\end{eqnarray}


\item[$t_x < t_1$]\hfill\\ If $t_x < t_1$, then all Material is
  ``future'' Material. We can only process it as fast as it comes in,
  or slower, but not faster and we can only start at $t_1$ but not
  earlier.

\begin{eqnarray}
tp'   &=& min (tpMax, tpReq) \\
\notag\\
t_x'  &=& t_1 + \frac{(t_2-t_1) tpReq}{tp'}
\end{eqnarray}

\item[$t_1 < t_x < t_0$]\hfill\\ There is a third case, namely when
  $t_1 < t_x < t_0$. This can only happen if $k<1$, i.e. when we can
  process faster than required (blue lines). Here we end up with two
  parts. One can be processed at full speed $tpMax$ like ``past''
  Material and the second can be processed at $tpReq$.

\begin{eqnarray}
tp'   &=& tpMax \\
t_x'  &=& \frac{t_x tpMax - t_1 tpReq}{tpMax - tpReq}\\
\notag\\
tp''   &=& tpReq \\
t_x''  &=& t_2
\end{eqnarray}
\end{description} 


In oder to obtain nicely aligned Streams we first get a set of
interesting times, i.e. the union of all start and end-times. It turns
out that rounding times is crucial, because otherwise rounding may
cause small gaps where there is no throuhgput at all, or overlaps with
a spike in throughput.

\begin{code}
-- get all the times, i.e.  where Segments start or end
stTimes :: Stream c -> [Time]
\end{code}

%if source
\begin{code}
stTimes s = nub $ sort $ concat $ map times s
  where
    times (b, (t1, t2), tp) = [t1,t2]

stRoundTimes :: Stream c -> Stream c
stRoundTimes st = map rnd st
  where
    rnd (b, (t1,t2), tp) = (b, (round4 t1, round4 t2), tp)
    round4 x = (fromIntegral (round (10000.0 * x) ::Int)) / 10000.0
\end{code}
%endif

First we partition the Stream, such that segements no longer overlap,
i.e.  have either matching start and end times or do not share a
common point in time. Then we group the Segments into groups with
matching start and end times. 

\begin{code}
stPartition :: Stream c -> Stream c
stGroup     :: Stream c -> [Stream c]
stThrottle  :: Throughput -> Stream c -> Stream c
\end{code}

%if source
\begin{code}
stPartition sgs = partition (stTimes sgs) sgs
  where
    partition [] sgs = sgs
    partition (t:ts) sgs = partition ts (seSplitAll t sgs)

stGroup st = (group . stSortByStart) $ stPartition st
  where
    group = groupBy ((==) `on` sgEndTime) 
\end{code}

\needspace{60em}
Finally here is the core of the throttle operation
\begin{code}
stThrottle tpMax st = stRoundTimes $ 
                      fst $ 
                      foldl' (throttleGroup tpMax) ([],-100) (stGroup st)

throttleGroup :: Throughput -> (Stream c,Time) -> Stream c -> (Stream c,Time)
throttleGroup tpMax (s,tx) sgs
  -- future Material
  | tx <= t1           = let tp' = min tpReq tpMax
                             k   = tpReq/tp'
                             tx' = t1 + k*(t2-t1)
                             s'  = stScaleTp k sgs
                         in (s ++ s', tx')
  -- middle Material           
  | t1 < tx && tx < t0 = let tp' = tpMax
                             k   = tpReq/tp'
                             tx' = (tx * tpMax - t1*tpReq)/(tpMax - tpReq)
                             s'  = stSetInterval (tx,tx') $ stScaleTp k sgs
                             -- future
                             tp'' = tpReq
                             tx'' = t2
                             s'' = stSetStartTime tx' sgs
                         in (s ++ s' ++ s'', t2)
  -- past material
  | tx >= t0           = let tp' = tpMax
                             k   = tpReq/tp'
                             tx' = tx + k*(t2-t1)
                             s'  = stDelay (tx - t1) $ stScaleTp k sgs
                         in (s ++ s', tx')
                             
  where
    -- all t1,t2 are the same, so we pick the first Segment in a group
    (t1,t2) = sgInterval $ head sgs
    tpReq   = sum $  map sgTp sgs 
    t0      = t2 - (t2-t1)*tpReq/tpMax
                      

\end{code}
%endif


When we apply a Throttle we expect the throughput to never exceed the
\verb!tpMax! imposed by \verb!Throttle!. Furthermore we expect the
whole thing to take longer, the more we reduce \verb!tpMax!. 

This is indeed the case. Cutting off a little bit off the top only has
a small effect on the duration, but more agressive reductions extend
the duration significantly and gradually turn the S-shaped volume
build-up into a straight line. 

Also note how the original Segments are chopped into small pieces due
to the \verb!stPartition! operation.

This could be an indication that \verb!Batches! become less and less
important the further down the processing chain you look. As an extreme
case you may consider mail from a collection office a
\verb!Batch!. Once mail gets delivered, the mailman couldn't care less
from which collection office his letters originated. It appears you
have to \emph{re-batch} occasionally to maintain meaningful batches.

\begin{figure}[htb!]
\centering
\caption{The effect of a Throttle from 10 to 8 (top) and 4 (bottom)}
\label{fig:throttle84}
\includegraphics[width=10cm]{throttle8.png}
\includegraphics[width=10cm]{throttle4.png}
\end{figure}

\needspace{30em}
\section{A complete Example}
\subsection{Material}
Let's construct some material. In the real world, this would be given.

\begin{code}
-- 267 city codes
sfCityCodes = map (printf "%03d") ([100,103..900]::[Int]) :: [String]

sfFlyerMaterial = M $ map f sfCityCodes
  where
    f code = ((Std, Flyer, To code),v, NoMaterial)
    v      = 10

-- bags containing flyers for all city codes
sfMessyFlyerMaterial :: Material SfClassification
sfMessyFlyerMaterial = M [((Std, Bag, To "sorting"), 8, sfFlyerMaterial)]

-- bags containing flyers for a single city code
sfSortedFlyerMaterial :: Material SfClassification
sfSortedFlyerMaterial = M $ map f sfCityCodes
  where
    f code = ((Std, Bag, To code), vBag, flyers code)
    flyers code = M [((Std, Flyer, To code), vFly, NoMaterial)]
    vBag   = 1
    vFly   = 100

-- individual parcels
sfParcelMaterial :: Material SfClassification
sfParcelMaterial = M $ map f sfCityCodes
  where
    f code = ((Std, Parcel, To code),v, NoMaterial)
    v      = 10
\end{code}

\subsection{Batches}
The actual Batches contain a mix of all the Material. Let's create a
shorthand function to create one

\begin{code}
mkSfBatch1 :: String -> Batch SfClassification
mkSfBatch1 name = (name, maMergeAll [
                      sfParcelMaterial,
                      sfSortedFlyerMaterial,
                      sfMessyFlyerMaterial])
\end{code}

\subsection{Streams}

The topology of the network is incoded in the way we construct
Steams. Let's start with the unbuffer processes which create the input
Streams.

\begin{code}
sfInStream1 :: Stream SfClassification
sfInStream1 = unbuffer (mkSfBatch1 "b1") (1,100) 100

sfInStream2 :: Stream SfClassification
sfInStream2 = unbuffer (mkSfBatch1 "b2") (80,150) 100

\end{code}

There are several splitting processes for which we'll now define
Filter functions. This information would come from a planning system
like Opal.

Splitting sorted from unsorted Flyer Bags and Parcels:
\begin{code}
sfIsMessyFlyerBag :: Filter SfClassification
sfIsMessyFlyerBag (cl, fo, To ds) = fo == Bag && ds == "sorting"

sfIsSortedFlyerBag :: Filter SfClassification
sfIsSortedFlyerBag (cl, fo, To ds) = fo == Bag && ds /= "sorting"

sfIsParcel :: Filter SfClassification
sfIsParcel (cl, fo, To ds) = fo ==  Parcel

\end{code}

Splitting by City Code:

\begin{code}
sfHasCityCode :: String -> Filter SfClassification
sfHasCityCode code (cl,fo,To ds) = ds == code
\end{code}

Splitting by Transport Direction requires knowledge how City Codes are
grouped to Transport Direction. We make this up.

\begin{code}
sfHasTransportDirection :: String -> Filter SfClassification
-- just group by first two digits
sfHasTransportDirection code (cl,fo,To ds) = (take 2 ds) == (take 2 code)
\end{code}

The in-Streams get split in three

\begin{code}
(messyFlyerStream1, sortedFlyerStream1, parcelStream1)
  = (
  stFilter sfIsMessyFlyerBag  sfInStream1,
  stFilter sfIsSortedFlyerBag sfInStream1,
  stFilter sfIsParcel         sfInStream1
  )

(messyFlyerStream2, sortedFlyerStream2, parcelStream2)
  = (
  stFilter sfIsMessyFlyerBag   sfInStream2,
  stFilter sfIsSortedFlyerBag  sfInStream2,
  stFilter sfIsParcel          sfInStream2
  )
\end{code}

The parcel sorter receives the sorted flyer bags and the parcels after
a short delay. The flyer sorter receives the unpacked unsorted flyer
bags. Both have limited thourghputs

\begin{code}
parcelSorterInput = stDelay 11 $
                    stThrottle 50 $
                    stMergeAll [
                      sortedFlyerStream1,
                      sortedFlyerStream2,
                      parcelStream1,
                      parcelStream2
                      ]

flyerSorterInput = stDelay 13 $
                   stThrottle 300 $ 
                   stUnpack $
                   stMergeAll [
                     messyFlyerStream1,
                     messyFlyerStream2
                     ]
\end{code}

\needspace{20em}
Xxx I am ignoring the Flyers which travel from the flyer sorter to the
parcel sorter (due to the problem of the pack function, and because I
am running oout of time).

But we can at least look at one output of the parcel sorter. The
result looks credible at first glance. We created \verb!sfInStream2!
to start much later than the first input Stream. This creates a gap,
where the sorter has nothing to do. But I really doubt that everything
is correct. This needs further investigation.


\begin{code}
parcelSorterOutput10 =
  stFilter (sfHasTransportDirection "10") $ 
  parcelSorterInput
\end{code}

\begin{figure}[htb!]
\centering
\caption{Parcel Sorter Transport Direction 10}
\label{fig:erd}
\includegraphics[width=10cm]{parcel10.png}
\end{figure}



\section{Discussion}

The method described here uses a simulation model which is a "simple"
formula. The formula is derived from process knowledge, i.e. the
topology of the network inside the hub and thoughputs, start/stop
times and split criteria.

\subsection{Events}

This simulation works without any events. This has a number of
consequences: 

\begin{itemize}
\item The network is static. You cannot easily start and stop a
      process. It should however be possible to implement a
      \verb!Throttle! which also has start and end times.
\item There are no explicit buffers. \verb!Throttle! maintains buffers
      as it does its computation, but they are not available to other
      computations. You can implicity compute buffer levels by
      comparing the Stream before and after a \verb!Throttle!

      It may be a better idea to explicitly maintain
      buffers as part of a Stream, such that a Stream is described by
      a buffer and a time plus a flow of Material.
\item There is currently no way to allocate and free resources, let
      alone start a process once the required resources are
      available. 
\item It is difficult to start a process once a sufficient amount of
      Material is available. However, Opal currently uses a
      \emph{planning workflow} to respond to predicted states. This
      means you run a simulation and then fix the problems you see. The
      planning workflow may automate this to some extent. All in all
      this is not a bad idea, because it keeps the simulation simple
      and allows responding to ``future'' situations. A pure DES would
      have trouble doing this.
\item If more than one process picks up material at the same location,
      then Material may be doubled. There is no safeguard, that
      ensures that Material can only go in one direction. This may be
      circumvented with an intelligent \verb!Split!, which only sends
      Material to direction $n$ if it wasn't sent to any of the
      directions $0 .. n-1$.
\item Unlike a DES, this method is incapable of handling loops. You
      cannot express the situation, where a small percentage of
      material needs to be re-fed into a process it already travelled
      through earlier.
\end{itemize}


\subsection{Chaining of processes and Locations}

Ultimately we need to automatically create the formula from
information in our database. Ideally this is ``just work'', provided
all required information is available in the database. 

However, it appears there is a missing piece. A process which moves or
alters Material must know where to pick up Material and where to
dispose it. Some other process may pick up Material \emph{where} the
first disposed it. This effectively creates a chain of processes an
allows creating the afforementioned formula.

The remaining question is: how do we express \emph{where}. It sounds a
lot like \emph{Locations} and I believe adding Locations to our data
is sufficient to allow deriving the formula.

There aren't that many locations inside a hub. Imagine you ask a floor
manager ``where is my parcel?''. His answer could be ``in chute 12'' or
``at dock 42'', all of which are Locations.

It may be a good idea to add a small degere of abstraction, such that
his answers would be ``in the chute with the Beijing mail'' or ``at
the dock where the Beijing truck stands''. Thus you could re-arrange
chutes and docks without affecting the formula.

\subsection{Building the formula}

It turns out, that building the formula requires some work, even
though the elementary operations are all available. Look at the
complete example above.

\subsection{Reporting KPIs}

It is not obvious, what we actually want to report. We can look at all
measuring points are ask questions about what material will pass
through each point. However, this does not immediatly tell us whether
or not planning needs to be changed.

We need operations which particularly detect \emph{bad things}.


\subsection{Where will an item be?}

Ideally a simulation would allow predicting the future location of an
item. Our method allows answering questions like: ``If I have an item
with this classification which arrived at part of a given Batch, where
\emph{could} it be 30 minutes from now. The answer will always look
like ``well, there were 20 items of that classification in that batch,
10 of them will be at A, 5 of them will be at B and 5 will be at
C''. So we can only express probablities of of future item location.

This is actually a good thing and not a limitiation. Had we used an
item-based DES, then it would have predicited precise future
locations, but these predictions are not guaranteed to actually happen
in the real world. 

When you pick up an item from a pile of mail, then the best thing a
DES can do is pick a random item. But in the real world another item
may get picked. In a way a DES silently adds assumptions to the model.

Our model is more honest. Picking an item from a pile of 100 items
only means that 1 out of 100 are picked and 99 remain, but says
nothing about individual items.

\subsection{Merging}

A similar problem arises with merging. Our model merges Streams in a
fair way. No stream is given precedence over another. In the real
world things may be different. 

This however, is not a limitation of our method as such, but only a
consequence of how we implemented the merge and throttle
operations. Nothing keeps us from implemening a throttle, which does
not scale down all Segments uniformly but gives some precence over
others.


\begin{code}

\end{code}

\begin{code}

\end{code}

\begin{code}

\end{code}

\section{Appendices}
\subsection{Syntax}
\begin{description}[style=nextline]
\item[\verb!::!] A type declaration. Read \verb!x :: Int! as ``x has type Int''
\item[\verb!->!] The type of a function. Read \verb!a->b->c! as ``a function
  which takes an $a$ and a $b$ and returns a $c$.
\item[\verb!type!] A type synonym. Read \verb!type Count = Double! as ``Count
  is another name for Double''
\item[\verb!data!] A new type definition. Read \verb!data Class = Speed|Std!
  as ``Class is a type with the two values Speed and Std. Can be
  recursive.
\item[\verb!Batch c!] A Type created from another type, comparable to
  ``A Stack of Ints''. Read as ``a Batch of c'', where c is the type
  of classification.
\item[\verb![Int]!] The type ``list of Ints''
\item[\verb!(x:xs)!] A list whose first element is $x$. Read as ``an $x$ followed by more $xs$
\item[\verb!(a,b,c)!] A triplet. Can be used for types
  \verb!(Int,Int,Double)!, values \verb!(1,2,3.14)! and parameters
  \verb!(x,y,z)!
\item[\verb!z = f x y!] Function application. Traditionally written as $z=
  f(x,y)$
\item[\verb!f $ g x!] Chained function application. Avoids parentheses
  as in $f(g x)$. Traditionally written as $f(g(x))$
\item[\verb!f x y = ...!] Function definition. Traditionally writen as $f(x,y)
  \{\dots\}$
\item [\verb!| aBool = aValue!] A ``guard''. Read as ``if aBool then aValue''
\item [\verb!Ord c =>!] Read as ``provided the values of type c can be compared for greater/less than
\item [\verb!deriving!] Don't worry about this.
\end{description}




\end{document}
