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
\renewcommand{\texfamily}{\fontfamily{cmtex}\selectfont\small}


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
\author{Martin Drautzburg}
\title{Temporal Data}
\begin{document} \maketitle 

\begin{abstract}
I present a simple data structure |Temporal| which allows treating
values which change over time (at discrete points in time) almost like
scalar values.

By making |Temporal| an instance of several widely used typeclasses,
we get a lot of power for free.
\end{abstract}


\tableofcontents 
\listoffigures
\listoftables

% ------------------------------------------------------------
\section{Temporal Data}
% ------------------------------------------------------------

A |Temporal| value is a value which changes over time at discrete
points in time. You can see a TV-channel as a |Temporal Image|. When
you switch channels with your remote, you get a |Temporal
Channel|. But since |Channel| is a |Temporal Image|, you'll end up
with a |Temporal (Temporal Image)|.

Obviously Temporal can be nested. You can flatten a |Temporal
Temporal| and in the TV-example you get a |Temporal Image| which will
represents what you acutually see on the screen.

|Temporal| itself is not a type. It is something, which takes a type
and creates a new type. This is akin to constructions like |Stack| or
|List| which by themselves are not types either. A Stack of Int
however, is a type. Hence you can turn any Type into its Temporal
counterpart.

We will construct |Temporal| from a default value and a list of
changes. A Temporal without any changes behaves mostly like a plain
time-invariant value. 

% ------------------------------------------------------------
\subsection{Changes}

A Change associates a point in time with some arbitrary value of any
type. In that respect it looks a lot like an \emph{Event}. However
with two succesive Changes having the same value, the second Change
has no effect. This is different from Events, where e.g. two
successive button clicks are different from just one.

\needspace{10em}
The definition below implicitly creates two functions |ct| and |cv|
which extract the time or the value from a change respectivly.

%if False
\begin{code}
module Temporal where
import Data.List.Ordered
import qualified Data.Map.Lazy as M
import Control.Monad
import qualified Data.DList as D
import Control.Applicative
import qualified Data.Foldable as F
import System.TimeIt
import Debug.Trace
\end{code}
%endif

\begin{code}
type Time = Integer
data Change a = Chg {
            ct :: Time, -- "change time"
            cv :: a     -- "change value"
        }

-- make the String-representation less vebose
instance Show a => Show (Change a) where
        show (Chg t v) = "(" ++ show t ++ "," ++ show v ++ ")"
\end{code}

\subsubsection{Some handy functions}

\begin{code}
-- "value function, returning change" - apply a function to the value, return Change
cvfc :: (a -> b) -> Change a -> Change b
cvfc f e = Chg (ct e) (f $ cv e)

-- "time function" - apply a function to the time, return Change
ctfc :: (Time -> Time) -> Change a -> Change a
ctfc f e = Chg (f $ ct e) (cv e)


-- "time function" - apply a function to the time, return a
ctf :: (Time -> b) -> Change a -> b
ctf f e = f $ ct e


\end{code}


\subsubsection{Lists of Changes}
From a List of Changes we can ask the next change time, or error or empty list
\begin{code}
ctNext :: [Change a] -> Time
ctNext = ct . head
\end{code}

\begin{code}
cDelay :: Time -> [Change a] -> [Change a]
cDelay t cgs = map (ctfc (+t)) cgs
\end{code}

We anticipate that we eventually have to remove duplicate
values which belong to successive Changes. We leave the question open,
how to test for equality, i.e. we might want to remove values which
are just \emph{close enough}. Hence the function which determines
whether or not two values are equal enough is a parameter to
|cNubBy| and so is an initial value |xd| to comapare with.

Nubbing does change the semantics of a Temporal somewhat, namely when
you insert another change between two equal changes.

\begin{code}
-- remove successive duplicate values from a list of Changes
cNubBy :: (a->a->Bool) -> a -> [Change a] -> [Change a]
cNubBy eq xd [] = []
cNubBy eq xd (e:es) 
       | eq (cv e) xd = cNubBy eq xd es
       | otherwise    = e : cNubBy eq (cv e)  es

\end{code}

The list of changes needs to be kept ordered at all times. This allows
us to slice a list changes by specifying two time predicates
representing lower and upper conditions. The resulting list contains
all changes where both predicates hold. Once the upper predicate
returns false we don't have to traverse the list any further.

we allow predicates for the whole change, or just its time component.

For time-slicing, We use predicates instead of Times so we can easily
slice with just a lower bound (think: \emph{sliceFrom}) or just an
upper bound and using |anytime| for the respective other bound.

\begin{code}
-- remove initial part of the list, 
-- return last discarded change value, or parameter c
cDropwhile  :: (Change a -> Bool) -> a -> [Change a] -> (a, [Change a])
cDropwhile _ c []    =  (c, [])
cDropwhile p c xs@(x:xs')
        | p x       =  cDropwhile p (cv x) xs'
        | otherwise =  (c,xs)

cTdropwhile  :: (Time -> Bool) -> a -> [Change a] -> (a, [Change a])
cTdropwhile p c =  cDropwhile (ctf p) c


cTslice :: (Time->Bool) -> (Time->Bool) -> [Change a] -> [Change a]
cTslice p1 p2 chgs = let (_,cs) = cTdropwhile (not. p1) undefined chgs
                     in takeWhile (ctf p2) cs


anytime = const True
\end{code}

For our work we will have to align two lists of Changes in a meaningful
way. If both lists have Changes at the same times, this is trivial. 

But we must also be prepared to find a Change in one list, where there
is no corresponding Change in the other list. In that case we must
create a new change in that other list, so we have enough changes to
pair them up.

\begin{figure}[htb!]
\centering
\includegraphics[width=6cm]{align.png}
\caption{Aligning changes}
\end{figure}

The value between two changes will be that of the earlier change. This
leaves a question about the value \emph{before} the first change. To
take care of that we pass |xd| and |yd| as values to |eAlgin| which
shall hold between the dawn of time and the first change.

\needspace{20em}
\begin{code}

eAlign :: (a,b) -> [Change a] -> [Change b] -> ([Change (a,b)])

-- if one of the lists is empty, them pair up with its constant
eAlign (xd,yd) cxs []  = map (cvfc (\xi -> (xi,yd))) cxs
eAlign (xd,yd) [] cys  = map (cvfc (\yi -> (xd,yi))) cys

-- Otherwise the result depends on which change is earlier
eAlign (xd,yd) cxss@((Chg tx xi):cxs) cyss@((Chg ty yi):cys) 
        | tx == ty = let c = (xi,yi)
                     in Chg ty c : eAlign c cxs  cys  
        | tx <  ty = let c = (xi,yd)
                     in Chg tx c : eAlign c cxs  cyss 
        | tx >  ty = let c = (xd, yi)
                     in Chg ty c : eAlign c cxss cys 
\end{code}

We'll use this function later on to define the |<*>|
\emph{Applicative} operation on Temporals. It is not strictly needed
but provides better performance than the alternative |ap|. See
(\ref{applicative})

% ------------------------------------------------------------
\subsection{Temporal}

Temporal combines a Change-list and a constant default value (often
named |xd| or |yd| into one data type. These two components can be
extracted by the implicitly created functions |tc| and |te|.

\begin{code}
data Temporal a = Temporal {
    td :: a,         -- "temporal default"
    tc :: [Change a] -- "temporal changes"
} deriving (Show)


-- get change values including the default
tChangeValues :: Temporal a -> [a]
tChangeValues tpr = td tpr : map cv (tc tpr)

\end{code}

\subsubsection{Some handy functions}\label{handy}

Temporals can be limited by Time. You can discard old changes and only
keep the ones which occur \emph{after} a certain point in time
(including that point in time). Or you can select only those which
fall into a given time window, where the upper bound is excluded.


\begin{code}
-- limit temporal to $t1 \leq te$
tAfter :: Time -> Temporal a -> Temporal a
tAfter t1 (Temporal xd xs) = Temporal c late
        where
            (c,late)  = cTdropwhile (<= t1) xd xs

-- limit temporal to $t1 \leq te < t2$
tWindow :: (Time, Time) -> Temporal a -> Temporal a
tWindow (t1, t2) tpr = let Temporal xd xs = tAfter t1 tpr
                       in  Temporal xd (takeWhile ((< t2) . ct) xs)


tDelay :: Time -> Temporal a -> Temporal a
tDelay t (Temporal d cs) = Temporal d (cDelay t cs)

\end{code}
With these definition we can now implement functions which give you
the value at a specific point time. It does not matter, whether or not
there is a Change at that point in time. In case there is no Change,
the value of the preceding Change or the default constant will
determine the value.

The two functions below only differ in the order of their parameters. 

\begin{code}
tAt :: Time -> Temporal a -> a
tAt t = td . (tWindow (t,t))

-- turn a Teporal into a function over time
tFt :: Temporal a -> (Time -> a)
tFt = flip tAt
\end{code}

Removing duplicates can be implemented in terms of |cNubBy|

\begin{code}
tNubBy :: (a->a->Bool) -> Temporal a -> Temporal a
tNubBy eq (Temporal xd xs) = Temporal xd (cNubBy eq xd xs)

\end{code}


% ------------------------------------------------------------
\subsection{Instances}

We will now make |Temporal| an instance of several widely used
typeclasses. This buys us a lot of operations without having to write
any code.

Let's first define some example |Temporal|s and a shorthand for
showing its first changes.

\begin{code}
-- Natural numbers occurring at even times
exNat :: Temporal Int
exNat  = Temporal 0 (map (\x -> Chg x (fromIntegral x)) [1..100000])

-- A Temporal which drops from 10 to zero
ext2 :: Temporal Int
ext2   = Temporal 10 [Chg 5 0]

-- Show the first changes only
exShow (Temporal d cgs) = Temporal d (take 6 cgs)

\end{code}

\subsubsection{Foldable}

The |Foldable| instace allows us to accumulate the values of a
|Temporal| using functions like |foldr| or |foldl| which take a
function (e.g |(+)|) and the intial value of the accumulator.

\begin{code}
instance F.Foldable Temporal where
          foldr f z tpr = Prelude.foldr f z (tChangeValues tpr)
\end{code}

A Fold allows us e.g. to sum up all values.

|*Main> F.foldr (+) 0 exNat|\\
  \eval{F.foldr (+) 0 exNat}

Obviously |F.foldr (+) 0| is the same as |sum|, but the package
|Data.Foldable| knows this too and we get |sum| for free.

|*Main> F.sum exNat|\\
  \eval{F.sum exNat}

\subsubsection{Functor}

The |Functor| instance allows us to map an unary function over a
|Temporal|

\begin{code}

instance Functor Temporal where
        fmap f (Temporal xd xs) = Temporal (f xd) (map (cvfc f) xs)
\end{code}

We can e.g. now convert the ascending natural numbers in |exNat| into
squares, which occur at the same times.

|*Main> exShow $  fmap (^2) exNat|\\
  \eval{exShow $  fmap (^2) exNat}

\subsubsection{Monad}

You can imagine a |Monad| as something which can be nested and
flattened using a function typically called |join|. A |List| is a
Monad, because you can have Lists of Lists and you can flatten a List
of Lists into a List. In our case we'll have to deal with Temporal
Temporals.

We define a the Monad instance with respect to the flattening function
|tJoin|. At its heart is |joinChs| to handle a List of |Change
(Temporal a)|. The outer Change has a change-time, and tJoin takes the
default from the embedded Temporal and schedules it to the
change-time, which turns it into a plain Change. Then it takes the
Changes of the embedded Temporal and filters those which occur beween
this change-time and the next change-time. The remaining code deals
with things at the beginning or end of the flattening.

\begin{figure}[htb!]
\centering
\includegraphics[width=8cm]{tJoin.png}
\caption{Flattening Temporals}
\end{figure}

\needspace{20em}
\begin{code}
tJoin :: Temporal (Temporal a) -> Temporal a
tJoin (Temporal tdef [])  = Temporal (td tdef) (tc tdef)
tJoin (Temporal tdef tchs) = Temporal (td tdef) (dChs ++ joinChs tchs)
        where
            dChs = let t2 = ctNext tchs -- next change in outer
                   in cTslice anytime (< t2) (tc tdef)

            joinChs :: [Change (Temporal a)] -> [(Change a)]
            -- last change
            joinChs ((Chg t1 tpr) : []) 
                    = tSchedBetween t1 anytime tpr
            -- other change
            joinChs ((Chg t1 tpr):tchs) 
                    = let t2 = ctNext tchs -- next change in outer
                      in tSchedBetween t1 (< t2) tpr ++ joinChs tchs

            -- from t1 until p2 becomes false
            tSchedBetween t1 p2 (Temporal tdef []) = [(Chg t1 tdef)] 
            tSchedBetween t1 p2 (Temporal tdef tchs)
                    | needsDefault  = Chg t1 tdef : changes
                    | otherwise     = changes
                    where
                        changes      = cTslice (>= t1) p2 tchs
                        needsDefault = null changes 
                                       || (t1 < (ctNext changes))

\end{code}

\begin{code}
exNested1 = Temporal ext2 [Chg 20 ext2]
exNested2 = Temporal exNat [Chg 7 ext2]
\end{code}

Once |tJoin| is defined we can now define the |monad| operations. If
you are unfamiliar with haskell, then this may appear quite strange to
you. Suffice it to say, that this is akin to solving quadratic
equations. Once you know the formula, there is need to execute the
steps to solve it time and again. Likewise the fact that |>>=| can be
written with respect to |join| and |fmap| is a thing which is always
true.

\begin{code}
instance Monad Temporal where
        return x    = Temporal x []
        ma >>= f    = tJoin $ fmap f ma
\end{code}


\subsubsection{Applicative}\label{applicative}

Finally the |Applicative| instance allows us to do similar things, but
no longer limited to unary functions. 

\begin{code}
instance Applicative Temporal where
        pure x = Temporal x []
        -- This is the shorter implementation  
        --        |(<*>)  = ap|

        -- This is 2..3 times faster
        (Temporal fc fs) <*> (Temporal xd xs) 
                = Temporal (fc xd) (map apply $ eAlign (fc,xd) fs xs )
                  where
                      apply (Chg t (f,x)) = Chg t (f x)

\end{code}

When we multiply |exNat| with |ext2| we expect an intial value of
$0$, because the natural numbers have a $tc=0$.

|ext2| however has a constant of 10, so we expect the natural
numbers to be multiplied by $10$. But that should happen only until we
hit the first (and only) change of |ext2|, which occurs at $t=5$. So
at $t=5$ the result should drop to zero and remain zero afterwards,
because there are no further changes in |ext2|


|*Main> exShow $ (*) <$> exNat <*> ext2|\\
  \eval{exShow $ (*) <$> exNat <*> ext2}

We can verify that indeed none of the remaining values is different
from zero by using |tNubBy| which sould remove the duplicate zeros.

|*Main> tNubBy (==) $ (*) <$> exNat <*> ext2|\\
  \eval{tNubBy (==) $ (*) <$> exNat <*> ext2}


The magic operators here are |<$>| and |<*>| where |<$>| separates the
function (here |(*)|) from its arguments and |<*>| separates the
arguments from each other. So |Temporal|s are only \emph{almost} like
scalars, because you have to use operators you wouldn't have to use
with scalars.

Needless to say, that this mechanism does not only work with |Int|s
but with |Temporal|s of any kind. Likewise the function is not limited
to |(*)|, or functions which operate on Ints or functions with two
parameters. The only requirement is that the types of the function
parameters match the type of the Temporals

\subsubsection{Perfomance}

Performance looks okay so far. In the example where the two
|Temporal|s are multiplied, we have to look at all $100.000$ changes
and this takes much less than a second.

\begingroup\small
\begin{verbatim} 
*Main> timeIt $ putStrLn $ show $ tNubBy (==) $ (*) <$> exNat <*> ext2
Temporal {td = 0, tc = [(1,10),(2,20),(3,30),(4,40),(5,0)]}
CPU time:   0.35s (interpreted)
CPU time:   0.06s (compiled)
\end{verbatim}
\endgroup
\subsubsection{Periodic Temporals}

It would be quite nice to specify a Temporal which shows some periodic
behavior. We must however, ask ourselves what happens when we repeat a
|Temporal| which takes longer than the time between repetitions. What
if we repeat the same song every minute, but the song takes three
minutes? Certainly we cannot simply deleay and repeat the Changes.

Luckily |tJoin| already takes care of this. All we need to do is
create a |Temporal Temporal| where the outer Temporal stands for the
repetitions and its Changes are the original Temporal, repeatedly
scheduled for a certain delay.

Let's first start with repeating Temporals at specific points in time,
which are teken from an ordered List.

\begin{code}
tRepeat :: [Time] -> Temporal a -> Temporal a
tRepeat times tpr = let changes = map (sched tpr) times
                in tJoin $ Temporal tpr changes
        where
            sched :: Temporal a -> Time -> Change (Temporal a)
            sched tpr' t = Chg t (Temporal (td tpr') (cDelay t (tc tpr')))

\end{code}

If we want to repeat at fixed time intervals, we do it like this:

\begin{code}
tCycle :: Time -> Temporal a -> Temporal a
tCycle t = tRepeat [t, 2*t ..]
\end{code}

Temporals created by |tCycle| have an infinite list of Changes. They
oscillate for all eternity, but this in not a problem in a lazy labguage.

The most general case is scheduling \emph{different} Temporals to
become effective at different times.xxx




Watch how |ext2| repeats itself every 12 units of time.

|*Main> exShow (tCycle 12 ext2)|\\
  \eval{exShow (tCycle 12 ext2)}

If we repeat with a higher frequeny, |ext2| can no longer drop to
zero, because at that time the next repetition has already started.

|*Main> exShow (tCycle 4  ext2)|\\
  \eval{exShow (tCycle 4  ext2)}

The large |exNat| is also not a problem. Hey, this would even work if
the Temporal had an infinite number of changes.

|*Main> exShow (tCycle 3  exNat)|\\
  \eval{exShow (tCycle 3  exNat)}


\subsubsection{Temporal Collections}

So far we only dealt with Temporals as time-varying values. We could
e.g. model a train by its time varying position. A Collection of such
trains would make a train schedule. 

But the schedule is no longer a Temporal, but a Collection of
Temporals. Our operations on Temporals like |tCycle| are not
directly applicable to the schedule.

Still a Collection of Temporals is semantically a Temporal. After all
it is a Value (a Collection of train positions) which changes over
time. It would be a cool thing to have an operation, which transforms
a Collection of Temporals into a Temporal Collection, particularly if
we don't have to be explicit about the type of the Collection (List,
Hashmap).

Then again, we would already be happy, if we could just apply
operations like |tCycle| on a Collection of Temporals and get a
Collection of Temporals back. Thus the |Temporal Collection| would
never have to materialize. It turns out that |fmap| already does most
of what we want. This will work on any collection which are instances
of |Functor|.

\begin{code}
exTrains1 :: M.Map String (Temporal String)
exTrains1 = M.insert "IC102" (Temporal 
                              "Konstanz" 
                              [
                               (Chg 10 "Radolfzell"),
                               (Chg 20 "Singen")
                              ]) 
            $
            M.insert "IC111" (Temporal 
                              "Mannheim" 
                              [
                               (Chg 25 "Karlsruhe"),
                               (Chg 40 "Offenburg")
                              ]) 
            $
            M.empty
\end{code}

We can now apply some our temporal functions. Let's first let the
train schedule repeat itself every 100 units of time

\begin{code}
exTrains2 = fmap (tCycle 100) exTrains1
\end{code}

Let's check where our tains are at a certain time.

|*Main> fmap (tAt 30) exTrains1|\\
  \eval{fmap (tAt 30) exTrains1}

|*Main> fmap (tAt 30) exTrains2|\\
  \eval{fmap (tAt 30) exTrains2}

|ExTrains1| does not include a repetition, so in the late future all
trains are at their final destination.

|*Main> fmap (tAt 100) exTrains1|\\
  \eval{fmap (tAt 100) exTrains1}

|ExTrains2| does include a repetition, so trains start all over again.

|*Main> fmap (tAt 100) exTrains2|\\
  \eval{fmap (tAt 100) exTrains2}


% ------------------------------------------------------------
\subsection{Relational Database representation}

Assuming each entity is represented as a Table with a primary key, you
can turn any table into a temporal table, by adding a |Change| table for
each attribute and have the main table hold only the temporal default
for that attribute. 

Consider a table of trains, where the only attribute is the location
of each train. We assume a train does not have a default location and
the ``location'' attribute in the main table is null.

\begin{table}[h!]
\begin{tabular}{ll}
\hline
\multicolumn{2}{c}{Train} \\
\hline
PK & Location\\
\hline
RE2310 & null \\
IC112 & null \\
\hline
\end{tabular}
\quad\quad\quad\quad
\begin{tabular}{llll}
\hline
\multicolumn{4}{c}{TrainLocationChange} \\
\hline
PK & FK & Time & Value \\
\hline
1 & RE2310 & 10.03.2015 16:00 & Konstanz \\
2 & RE2310 & 10.03.2015 16:25 & Radolfzell \\
3 & RE2310 & 10.03.2015 16:40 & Singen \\
4 & RE2310 & 10.03.2015 16:45 & null \\
5 & IC112 & 10.03.2015 06:20 & Mannheim \\
6 & IC112 & 10.03.2015 07:00 & Karlsruhe \\
7 & IC112 & 10.03.2015 08:40 & Offenburg \\
\hline
\end{tabular}
\caption{Temporal relational table}
\end{table}

The primary key for TrainLocationChanges is not strictly needed, but
offers the possibility to reference a row from another temporal table,
creating a |Temporal Temporal|.

After 10.03.2015 16:45 the RE2310 is ``nowhere'', while the IC112
stays in Offenburg forever after 10.03.2015 08:40.

For selects from temporal tables, the following must be kept in mind:
\begin{itemize}
\item a select without a time restriction, or with a time-range
  restriction returns a temporal value. However, that cannot be
  expressed in a single relation, because we need a main table and a
  number of change tables.
\item It is however, possible to write several selects, one for the
  main table and one for each changing attribute of the result.
\item Denormalizing the result it not a good idea. Even a table with
  just two temporal attributes, would, when queried, result in all
  combinations of changes. These combinations are pretty meaningless. 
\item A select with a point-in-time restriction returns a simple
  relation. This follows directly from the implementation of |tAt| in
  (\ref{handy})
\item All in all, temporal values do not fit nicely into the relational
  paradigm.
\item Ideally one would have true temporal attributes, without having
  to resort to separate change tables.
\end{itemize}




%\begin{figure}[htb!]
%\centering
%\includegraphics[width=4cm]{glass-slipper.jpg}
%\end{figure}

\end{document}

(define-key outline-minor-mode-map [(C-left)] 'hide-sublevels) 
