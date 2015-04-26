\documentclass[a4paper]{article}
%include lhs2TeX.fmt
%include greek.fmt
%options ghci -fglasgow-exts
%format <- = "\char''30"

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
\newmdenv[frametitle=Example,skipabove=1em,backgroundcolor=gray!05,roundcorner=2pt]{run}
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

Obviously Temporals can be nested. You can flatten a |Temporal
Temporal| and in the TV-example you get a |Temporal Image| which will
represents what you acutually see on the screen.

|Temporal| itself is not a type. It is something, which takes a type
and creates a new type. This is akin to constructions like |Stack| or
|List| which by themselves are not types either. A Stack of Int
however, is a type. Hence you can turn any Type into its Temporal
counterpart.

%if False
\begin{code}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Temporal where
import Data.Maybe
import Data.List
import qualified Data.Map.Lazy as M
import Control.Monad
import Control.Applicative
import qualified Data.Foldable as F
import Data.Function

import System.TimeIt
import qualified Text.Show.Pretty as Pr
import Debug.Trace

bench x = (timeIt . print) x
pp x = putStrLn $ Pr.ppShow x
\end{code}
%endif

\subsection{Time}

We want Temporals to have defined values for all times. Interpreting
Temporals as a stream of changes, the value in the far future will be
the value of the last change. However, the value \emph{before} the
\emph{first} change is not so obvious. To cater for that, we introduce
a new type of |Time|, which has a distinct value |DPast| for the
\emph{distant past}, which is equal to itself, but smaller than any
other |Time|.

\begin{code}
data Time = DPast | T Integer deriving (Eq, Show)

instance Ord Time
        where
            compare DPast DPast = EQ
            compare DPast _     = LT
            compare _     DPast = GT
            compare (T t1) (T t2)  = compare t1 t2
\end{code}

Thus we get all operations which rely on comparing times for free.    

\begin{run}
|*Main> sort [T 42, DPast, T 41]|\\
  \eval{sort [T 42, DPast, T 41]}

|*Main> maximum [T 42, DPast, T 41]|\\
  \eval{maximum [T 42, DPast, T 41]}

\end{run}

We can use arithmetic operations on |Time|

\begin{code}
type DT = Integer
add :: DT -> Time -> Time
add _ DPast  = DPast
add dt (T t) = T (t + dt)
\end{code}

\subsection{Temporal}

The main property of a |Temporal| is that you can step through
it. There are many implementation options, but we chose the following:
with each \emph{fetch}, you get

\begin{itemize}
\item the value of the next change
\item the time where the next change occurs
\item a new |Temporal| or |Nothing|, i.e. a |Maybe Temporal|
\end{itemize}

This is what Lists do, and the new Temporal returned with each step is
simply the tail of the List. Hence we choose to use List as the
underlying structure.

\begin{code}
data Temporal a = Temporal [(Time, a)] deriving (Eq, Show)
\end{code}


\subsubsection{Creating}

A |Temporal| can be created from a |List| of |(Time, Value)| Pairs. To
discourage forgetting a |DPast| value we explicitly ask for a default
value. The remaining List elements mut be ordered by time. The list
can be infinite.


\begin{code}
temporal :: a -> [(Time, a)] -> Temporal a
temporal def chs = Temporal $ (DPast, def) : chs
\end{code}

\begin{run} 

We chose two examples |ex1| and |ex2|, where the value at |DPast| is 1
or 2 and the value at changes are always equal to the time, so they
are easy to remember. 

\begin{code}
ex1, ex2 :: Temporal Int
ex1   = temporal 1 [(T 3,3), (T 7, 7)]
ex2   = temporal 2 [(T 5,5), (T 9, 9)]
\end{code}
\end{run}

\subsubsection{List operations}

Since Temporals are essentially Lists we can lift any List operation
to operate of a Temporal.

\begin{code}
toList :: Temporal a -> [(Time, a)]
toList (Temporal xs) = xs

-- forces the first value to occur at |DPast|
fromList :: [(Time,a)] -> Temporal a
fromList ((t,v):xs) = temporal v xs

listLift  :: ([(Time, a1)] -> [(Time, a)]) -> Temporal a1 -> Temporal a
listLift f (Temporal chs) = Temporal $ f chs

tTakeWhile :: (Time->Bool) -> Temporal a -> Temporal a
tTakeWhile p = listLift (takeWhile (p.fst))

tUntil :: Time -> Temporal a -> Temporal a
tUntil t = tTakeWhile (< t)


tNub :: Eq a => Temporal a -> Temporal a
tNub  =  fromList . f . toList 
        where
            f [] = []
            f (x:xs) = x : f (dropWhile (eq x) xs)
            eq = (==) `on` snd

\end{code}


\subsubsection{Accessing}
\begin{code}

tVh :: Temporal a -> a
tVh = snd . head . toList        -- value head

tTt, tTh :: Temporal a -> Time
tTt = fst . head . tail . toList -- time tail
tTh = fst . head . toList        -- time head

tNull = null . toList

\end{code}

\subsubsection{Foldable}

It is possible to accumulate all values of a |Temporal| by providing a
function, like |+| or |*| and a starting value. Such things are called
|Foldable|.

\begin{code}
instance F.Foldable Temporal where
        foldr f z (Temporal chs) = let g (t,v) acc  = f v acc
                                   in F.foldr g z chs
\end{code}

\needspace{10em}
\begin{run}
|*Main> F.foldr (+) 0 ex1|\\
  \eval{F.foldr (+) 0 ex1}

|*Main> F.foldr (*) 1 ex1|\\
  \eval{F.foldr (*) 1 ex1}
\end{run}

\subsubsection{Functor}

A Functor is a thing, which allows applying (\emph{mapping}) a unary
function to each element, and |Temporal| is one of them.

\begin{code}
instance Functor Temporal where
  fmap f (Temporal chs) = let g (t,v) = (t, f v)
                          in Temporal (fmap g chs)

\end{code}

\begin{run}
|*Main> fmap (* 2) ex1|\\
  \eval{fmap (* 2) ex1}
\end{run}

\subsubsection{Applicative}

An |Applicative Functor| is like a |Functor| but not limited to unary
functions and it provides a special syntax. The tricky function to
implement is |<*>|. It must fetch next values from either of the two
Temporals, such that always the \emph{earlier} change is preferred
and the iteration ends, when there are no more changes.

\begin{figure}[htb!]
\centering \includegraphics[width=6cm]{align.png}
\caption{Aligning changes}
\end{figure}

\needspace{20em}
\begin{code}
instance Applicative Temporal where
        pure x = temporal x []
        Temporal fs <*> Temporal xs = Temporal $ fs `apply` xs
          where
            apply gs [] = []
            apply [] zs = []
            apply gs zs
              | gs `bef` zs = result gs zs : apply (tail gs) zs
              | zs `bef` gs = result gs zs : apply gs        (tail zs)
              | otherwise   = result gs zs : apply (tail gs) (tail zs)
            result ((ta,va):_) ((tb,vb):_)  = (max ta tb, va vb)
            bef as bs = let t       = fst  . head . tail
                            noTail  = null . tail
                            hasTail = not  . noTail
                        in hasTail as && (noTail bs || t as < t bs)
\end{code}

\needspace{20em}
\begin{run}
|*Main> ex1|\\
  \eval{ex1}

|*Main> ex2|\\
  \eval{ex2}

|*Main> (*) <$> ex1 <*> ex2|\\
  \eval{(*) <$> ex1 <*> ex2}
\end{run}

\subsubsection{Monad}

You can imagine a |Monad| as something which can be nested and
flattened. A |List| is a Monad, because you can have Lists of Lists
and you can flatten a List of Lists into a List. In our case we'll
have to deal with Temporal Temporals.

\begin{figure}[htb!]
\centering \includegraphics[width=4cm]{tJoin.png}
\caption{Temporal of Temporal}
\label{fig:tJoin}
\end{figure}

When we \emph{fmap} a function |a->Temoral b| over a |Temporal a| we
would get a |Temporal Temporal b|. To implement a Monad, we must
flatten this to a plain |Temporal b|

In Figure \ref{fig:tJoin} you see a |Temporal Temporal|. The inner
changes are connected by the black arrows, and the outer changes are
connected by the red arrows. We need to construct the blue
connections, which will be a plain |Temporal|.

\begin{code}
tBind :: (Temporal a) -> (a -> Temporal b) -> Temporal b
tBind tpr f
        | tNull tpr         = error "empty Temporal"
        | tNull (tTail tpr) = lates
        | otherwise         = let hd = (tUntil (tTt tpr) lates)
                              in hd `tAppend` (tTail tpr `tBind` f)

        where
            lates = switchAt (tTh tpr) ( f (tVh tpr))
            tTail = listLift tail
            tAppend as bs = Temporal $ toList as ++ toList bs
            switchAt t tpx
                    | tNull (tTail tpx)             = Temporal (tot tpx)
                    | between t (tTh tpx) (tTt tpx) = Temporal (tot tpx)
                    | otherwise                     = switchAt t (tTail tpx)
                    where
                        tot (Temporal ((ty,vy):xs)) = ((max t ty, vy):xs)
                        between t x y               = t >= x && t < y

instance Monad Temporal
         where return x = temporal x []
               m >>= f  = tBind m f
\end{code}

\subsubsection{Performance}

To get a feel about the performance, we "multilply" a very large
Temporal with one, that drops to zero. We expect that all result
values are zero after a certain point in time.


\begin{run}
\begin{code}
ex0   = temporal 2 [(T 3,5), (T 4, 0)]
exNat = temporal 0 [(T i, i) | i <- [1..100000]]
\end{code}

|*Main> timeIt $ putStrLn $ show $ tNub $ (*) <$> exNat <*> ex0|\\
  \eval{timeIt $ putStrLn $ show $ tNub $ (*) <$> exNat <*> ex0}

\end{run}

\subsection{Applications}
\subsubsection{Periodic Temporals}

It would be quite nice to specify a Temporal which shows some periodic
behavior. We must however, ask ourselves what happens when we repeat a
|Temporal| which takes longer than the time between repetitions. What
if we repeat the same song every minute, but the song takes three
minutes? Certainly we cannot simply delay and repeat the Changes.

Luckily |join| already takes care of this. All we need to do is
create a |Temporal Temporal| where the outer Temporal stands for the
repetitions and its Changes are the original Temporal, repeatedly
scheduled for a certain delay.

Let's first start with repeating Temporals at specific time-offsets,
which are teken from an ordered List. If that list is empty, the
original Temporal will be returned unchanged. 


\begin{code}
tDelayBy :: DT -> Temporal a -> Temporal a
tDelayBy dt = listLift (map delay)
        where
            delay (tx,vx) = (add dt tx, vx)

--tRepeat :: [DT] -> Temporal a -> Temporal a
--tRepeat times tpr =  join $ Temporal $ (DPast, tpr) : (map sched times)
--        where
--            sched tx = (T tx, tDelayBy tx tpr)

--tR :: [DT] -> Temporal a -> Temporal a
tR times tpr =   (DPast, tpr) : (map sched times)
        where
            sched tx = (T tx, tDelayBy tx tpr)

\end{code}

If we want to repeat at regular time intervals, we do it like this:

\begin{code}
-- tCycle :: DT -> Temporal a -> Temporal a
-- tCycle t = tRepeat [t, 2*t ..]


--ex10 :: Temporal Int
ex10 = show $ tUntil (T 5) $ outer `tBind` \_ -> ex1 :: Temporal Int
        where
            outer = fromList [(T (fromIntegral t), t)| t <- [5,10 ..]]
\end{code}


\end{document}

(define-key outline-minor-mode-map [(C-left)] 'hide-sublevels) 

\end{document}
