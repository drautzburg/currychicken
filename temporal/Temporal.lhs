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
\newmdenv[frametitle=Example,backgroundcolor=gray!05,roundcorner=2pt]{run}
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
--import qualified Text.Show.Pretty as Pr
import Debug.Trace

bench x = (timeIt . print) x
-- pp x = putStrLn $ Pr.ppShow x
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



\subsection{Temporal}

The main property of a |Temporal| is that you can step through
it. There are many implementation options, but we chose the following:
with each \emph{fetch}, you get

\begin{itemize}
\item the value of the next change
\item the time where the next change occurs
\item a new |Temporal| or |Nothing|, i.e. a |Maybe Temporal|
\end{itemize}

This is roughly equivalent to a List of |(Time,Value)| pairs, where
the new Temporal, which is returned with each fetch is the tail of the
List and the empty list you get at the end is replaced by |Nothing|.

However, we do not make any assumptions about the internal
representation, i.e. it does not have to be a List. Likewise, we do
not make any assumptions about the actual data, i.e. the type of the
thing, which changes over time.

\begin{code}
data Temporal a = Temporal [(Time, a)] deriving (Eq, Show)
\end{code}


\subsubsection{Creating}

A |Temporal| can be created from a |List| of |(Time, Value)|
Pairs. The list must be ordered by time and the first time must be
|DPast|. The list can be infinite. Should there be duplicate values
for the same |Time|, only the last value will survive.

\begin{code}
temporal :: a -> [(Time, a)] -> Temporal a
temporal def tps = Temporal $ (DPast, def) : tps
\end{code}

\begin{run} 

We chose two examples, where the value at |DPast| is 1 or 2 and the
value at changes are always equal to the time, so they are easy to
remember.

\begin{code}
ex1   = temporal 1 [(T 3,3), (T 7, 7)]
ex2   = temporal 2 [(T 5,5), (T 9, 0)]
exNat = temporal 0 [(T i, i) | i <- [1..]]
\end{code}
\end{run}


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
flattened using a function typically called |join|. A |List| is a
Monad, because you can have Lists of Lists and you can flatten a List
of Lists into a List. In our case we'll have to deal with Temporal
Temporals.

We define a the Monad instance with respect to the flattening function
|tJoin| which converts a Temporal Temporal into a mere Temporal,
i.e. it removes on level of nesting.

\begin{figure}[htb!]
\centering \includegraphics[width=4cm]{tJoin.png}
\caption{Temporal of Temporal}
\label{fig:tJoin}
\end{figure}

In Figure \ref{fig:tJoin} you see a |Temporal Temporal|. The inner
changes are connected by the black arrows, and the outer changes are
connected by the red arrows. The task of |tJoin| is to construct the
blue connections, which will be a plain |Temporal|.

The blue path must obey the following rules
\begin{itemize}
\item it starts with the very first (upper left) cell
\item it ends with the very last (lower right) cell
\item the times of the connected cells are ascending
\item the time of each cell lies before the time of the next outer change (if any)
\item all possible cells are included (no cell is needlessly omitted)
\item no times or values are altered
\end{itemize}

I did not manage to implement a Monad instance in a readable
way. This, and the fact I wouldnÄt know what to do with it, led be to
believe that a Monad instance is not strictly required.


\subsection{Other Operations}

Since Temporals are essentially Lists we can lift any List operation
to operate of a Temporal.

\begin{code}
listLift  :: ([(Time, a1)] -> [(Time, a)]) -> Temporal a1 -> Temporal a
listLift f tpr = Temporal $ f $ toList tpr
\end{code}

We can discard the initial part of a Temporal, such that the last
discarded values becomes the value of |DPast|.

\begin{code}

toList :: Temporal a -> [(Time, a)]
toList (Temporal xs) = xs

dpast :: Temporal a -> Temporal a
dpast (Temporal []) = error "empty Temporal"
dpast (Temporal xs) = temporal (snd $ head xs) (tail xs)

tDropWhile :: (Time->Bool) -> Temporal a -> Temporal a
tDropWhile p  = dpast . (listLift (dropWhile (p.fst)))

tTakeWhile :: (Time->Bool) -> Temporal a -> Temporal a
tTakeWhile p = dpast . (listLift (takeWhile (p.fst)))

tNub :: Eq a => Temporal a -> Temporal a
tNub = listLift (nubBy (on (==) snd))
\end{code}

\end{document}

(define-key outline-minor-mode-map [(C-left)] 'hide-sublevels) 

\end{document}
