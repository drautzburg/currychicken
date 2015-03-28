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

Nubbing does change the semantics of a Temporal somewhat, namely when
you insert another change between two equal changes.

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

We'll use this function later on to define higher-order functions on
Temporals. It is not strictly needed but provides better performance
than the alternative. See (\ref{applicative})

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

\subsubsection{Some handy functions}

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
exShow = tWindow (0,7)

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
        fmap f (Temporal xd xs) = Temporal (f xd) (map (cvf f) xs)
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
(Temporal a)|. The Change has a change-time, and tJoin takes the
default from the embedded Temporal and schedules it to the
change-time, which turns it into a plain Change. Then it takes the
Changes of the embedded Temporal and filters those which occur beween
this change-time and the next change-time. The remaining code deals
with things at the beginning or end of the flattening.

\begin{code}

tJoin :: Temporal (Temporal a) -> Temporal a
tJoin (Temporal tdef [])  = Temporal (td tdef) (tc tdef)
tJoin (Temporal tdef tchs) = Temporal (td tdef) (dChs ++ joinChs tchs)
        where
                  
            dChs = let t2 = (ct.head) tchs -- next change in outer
                   in tSchedTo t2 tdef

            joinChs :: [Change (Temporal a)] -> [(Change a)]
            joinChs ((Chg t1 tpr) : []) 
                    = tSchedFrom t1 tpr
            joinChs ((Chg t1 tpr):tchs) 
                    = let t2 = (ct.head) tchs -- next change in outer
                      in tSchedBetween t1 t2 tpr ++ joinChs tchs


            tSchedTo t2 (Temporal tdef tchs) = changes
                    where
                        changes = [c | c<-tchs, ct c < t2 ]

            tSchedBetween t1 t2 (Temporal tdef []) = [(Chg t1 tdef)] 
            tSchedBetween t1 t2 (Temporal tdef (tch:tchs))
                    | t1 < ct tch = Chg t1 tdef : changes
                    | otherwise   = changes
                    where
                        changes = [c | c<-(tch:tchs), ct c >= t1, ct c < t2 ]

            tSchedFrom t1 (Temporal tdef []) = [(Chg t1 tdef)]
            tSchedFrom t1 (Temporal tdef (tch:tchs))
                    | t1 < ct tch = Chg t1 tdef : changes
                    | otherwise   = changes
                    where
                        changes = [c | c<-(tch:tchs), ct c >= t1]



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
to |(*)|, functions which operate on Ints or functions with two
parameters. The only requirement is that the types of the function
parameters match the type of the Temporals

\subsubsection{Perfomance}

Performance looks okay so far. In the example where the two
|Temporal|s are multiplied, we have to look at all $100.000$ changes
and this takes much less than a second.

\begin{verbatim} 
*Main> timeIt $ putStrLn $ show $ tNubBy (==) $ (*) <$> exNat <*> ext2
Temporal {td = 0, tc = [(1,10),(2,20),(3,30),(4,40),(5,0)]}
CPU time:   0.35s (interpreted)
CPU time:   0.06s (compiled)
\end{verbatim}


\subsection{Applications}

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

\subsection{Operations}





%\begin{figure}[htb!]
%\centering
%\includegraphics[width=4cm]{glass-slipper.jpg}
%\end{figure}

\end{document}
