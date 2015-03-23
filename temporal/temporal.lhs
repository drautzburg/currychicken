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

\section{Temporal Data}

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
import Data.List.Ordered
import Control.Applicative
import qualified Data.Foldable as F
import System.TimeIt
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
-- apply a function to the value
cvf :: (a->b) -> Change a -> Change b
cvf f e = Chg (ct e) (f $ cv e)

-- apply a function to the time
ctf :: (Time->Time) -> Change a -> Change a
ctf f e = Chg (f $ ct e) (cv e)
\end{code}

We anticipate that we eventually have to remove duplicate
values which belong to successive Changes. We leave the question open,
how to test for equality, i.e. we might want to remove values which
are just \emph{close enough}. Hence the function which determines
whether or not two values are equal enough is a parameter to
|eNubBy| and so is an initial value |xd| to comapare with.

\begin{code}
-- remove successive duplicate values from a list of Changes
eNubBy :: (a->a->Bool) -> a -> [Change a] -> [Change a]
eNubBy eq xd [] = []
eNubBy eq xd (e:es) 
       | eq (cv e) xd = eNubBy eq xd es
       | otherwise    = e : eNubBy eq (cv e)  es

\end{code}

\subsubsection{Aligining}

For our work we will have to align two lists of Changes in a meaningful
way. If both lists have Changes at the same times, this is trivial. 

But we must also be prepared to find a Change in one list, where there
is no corresponding Change in the other list. In that case we must
create a new change in that other list, so we have enough changes to
pair them up.

The value between two changes will be that of the earlier change. This
leaves a question about the value \emph{before} the first change. To
take care of that we pass |xd| and |yd| as values to |eAlgin| which
shall hold between the dawn of time and the first change.

\needspace{20em}
\begin{code}

eAlign :: (a,b) -> [Change a] -> [Change b] -> ([Change (a,b)])

-- if one of the lists is empty, them pair up with its constant
eAlign (xd,yd) cxs []  = map (cvf (\xi -> (xi,yd))) cxs
eAlign (xd,yd) [] cys  = map (cvf (\yi -> (xd,yi))) cys

-- Otherwise the result depends on which change is earlier
eAlign (xd,yd) cxss@((Chg tx xi):cxs) cyss@((Chg ty yi):cys) 
        | tx == ty = let c = (xi,yi)
                     in Chg ty c : eAlign c cxs  cys  
        | tx <  ty = let c = (xi,yd)
                     in Chg tx c : eAlign c cxs  cyss 
        | tx >  ty = let c = (xd, yi)
                     in Chg ty c : eAlign c cxss cys 
\end{code}


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


\subsubsection{Time Windows}

Temporals can be limited by Time. You can discard old changes and only
keep the ones which occur \emph{after} a certain point in time
(including that point in time). Or you can select only those which
fall into a given time window, where the upper bound is excluded.


\begin{code}
-- limit temporal to $t1 \leq te$
tAfter :: Time -> Temporal a -> Temporal a
tAfter t1 (Temporal xd xs) = Temporal c late
        where
            (c,late)  = dropwhile ((<= t1)  . ct) xd xs

            -- like dropWhile but also return the final constant
            dropwhile  :: (Change a -> Bool) -> a -> [Change a] -> (a, [Change a])
            dropwhile _ c []    =  (c, [])
            dropwhile p c xs@(x:xs')
                    | p x       =  dropwhile p (cv x) xs'
                    | otherwise =  (c,xs)

-- limit temporal to $t1 \leq te < t2$
tWindow :: (Time, Time) -> Temporal a -> Temporal a
tWindow (t1, t2) tpr = let Temporal xd xs = tAfter t1 tpr
                       in  Temporal xd (takeWhile ((< t2) . ct) xs)


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

Removing duplicates can be implemented in terms of |eNubBy|

\begin{code}
tNubBy :: (a->a->Bool) -> Temporal a -> Temporal a
tNubBy eq (Temporal xd xs) = Temporal xd (eNubBy eq xd xs)

\end{code}

\subsubsection{Instances}
We will now make |Temporal| an instance of several widely used
typeclasses. This buys us a lot of operations without having to write
any code.

Let's first define some example |Temporal|s and a shorthand for
showing its first changes.

\begin{code}
-- Natural numbers occurring at even times
exNat  = Temporal 0 (map (\x -> Chg (2*x) x) [1..100000])

-- A Temporal which drops from 10 to zero
ext2   = Temporal 10 [Chg 5 0]

-- Show the first changes only
exShow = tWindow (0,10)

\end{code}

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


The |Functor| instance allows us to map an unary function over a
|Temporal|

\begin{code}

instance Functor Temporal where
        fmap f (Temporal xd xs) = Temporal (f xd) (map (cvf f) xs)
\end{code}

We can e.g. now convert the ascending natural numbers in |exNat| into
squares, which occur at the same times.

|*Main> exShow $  fmap (^2) exNat|\\
  \eval{exShow $  fmap (^2) exNat}


Finally the |Applicative| instance allows us to do similar things, but
no longer limited to unary functions. 

\begin{code}
instance Applicative Temporal where
        pure x = Temporal x []

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

Needless to say, that this mechanism doesn't only work with |Int|s but
with |Temporal|s of any kind. Likewise the function is not limited to
|(*)|, functions which operate on Ints or functions with two
parameters. The only requirement is that the types of the function
parameters match the type of the Temporals

\subsubsection{Periodic Changes}

It would be quite nice to specify a Temporal which shows some periodic
behavior. This is indeed possible and not very difficult, though I
wouldn't know how to do this in Java.

Here is a temporal whith a default of zero which repeats the values
|1,2,3| over and over.

\begin{code}
exPeriodic = Temporal 0 
             (
              let cs = (Chg 1 1):(Chg 2 2):(Chg 3 3): map (ctf $ (+3)) cs
              in  cs
             )
\end{code}

|*Main> exShow exPeriodic|\\
  \eval{exShow exPeriodic}

This one does not have any upper bound. It oscillates for all eternity.

\subsubsection{Perfomance}

Performance looks okay so far. In the example where the two
|Temporal|s are multiplied, we have to look at all $100.000$ changes
and this takes much less than a second.

\begin{verbatim}
*Main> timeIt $ putStrLn $ show $ tNubBy (==) $ (*) <$> exNat <*> ext2
Temporal {tc = 0, te = [(2,10),(4,20),(5,0)]}
CPU time:   0.35s

\end{verbatim}

%\begin{figure}[htb!]
%\centering
%\includegraphics[width=4cm]{glass-slipper.jpg}
%\end{figure}

\end{document}
