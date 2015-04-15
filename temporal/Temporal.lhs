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
import qualified Data.DList as D
import Control.Applicative
import qualified Data.Foldable as F
import System.TimeIt
import Debug.Trace
import Text.PrettyPrint.GenericPretty
import Data.Typeable

bench x = (timeIt . print) x
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
with each \emph{step}, you get

\begin{itemize}
\item the value of the next change
\item the time where the next change occurs
\item a new |Temporal| or |Nothing|, i.e. a |Maybe Temporal|
\end{itemize}

This is roughly equivalent to a List of |(Time,Value)| pairs, where
the new Temporal, which is returned with each step is the tail of the
List and the empty list you get at the end is replaced by |Nothing|.

However, we do not make any assumptions about the internal
representation, i.e. it does not have to be a List. Likewise, we do
not make any assumptions about the actual data, i.e. the type of the
thing, which changes over time.

\begin{code}
data Temporal a = Temporal {
            step :: (a, Time, Maybe (Temporal a))
        }
\end{code}

\subsubsection{Converting to and from Lists}

A |Temporal| can be created from a |List| of |(Time, Value)|
Pairs. The list must be ordered by time and the first time must be
|DPast|. The list can be infinite. Should there be duplicate values
for the same |Time|, only the last value will survive.

\begin{code}
fromList :: [(Time, a)] -> Temporal a
fromList [] = error "empty list"
fromList ((t,v):xs) = Temporal nxt
        where
            dupe = fst (head xs) == t
            nxt 
                    | null xs   = (v,t,Nothing)
                    | dupe      = step $ fromList xs
                    | otherwise = (v,t, Just $ fromList xs)
\end{code}

\begin{run} 

We chose two examples, where the value at |DPast| is 1 or 2 and the
value at changes are always equal to the time, so they are easy to
remember.

\begin{code}
ex1 = fromList [(DPast, 1), (T 3,3), (T 7, 7)]
ex2 = fromList [(DPast, 2), (T 5,5), (T 9, 9)]
\end{code}
\end{run}

A |Temporal| can be converted into a List. This is, among other
things, useful for printing a |Temporal|.

\begin{code}
-- apply a function to each (Time, Value) and accumulate the results
traverseBy :: (a -> Time -> b) -> Temporal a -> [b]
traverseBy f tpr = 
        let (vx, tx, mtp ) = step tpr
            tpr'           = fromJust mtp
            y              = f vx tx
        in case () of _
                              | isNothing mtp  -> [y]
                              | otherwise      -> y : traverseBy f tpr'


-- get all the change times
tTimes :: Temporal a -> [Time]
tTimes tpr = let f a t= t
             in traverseBy f tpr

-- get all the values
tValues :: Temporal a -> [a]
tValues = let f a t = a
          in traverseBy f 

toList :: Temporal a -> [(Time, a)]
toList tpr = zip (tTimes tpr) (tValues tpr)
\end{code}

\subsubsection{Eq and Show}

Testing two Temporals for equality will be primarily needed for
running tests. We're lazy here and resort to comparing the List
representations.

\begin{code}
instance (Eq a) => Eq (Temporal a) where
        tp1 == tp2 = toList tp1 == toList tp2
\end{code}

And this the equivalent of a |toString| method.

\begin{code}
instance (Show a) => Show (Temporal a) where
        show tpr = "fromList " ++ show (toList tpr)

\end{code}

\begin{run}
|*Main> ex1|\\
  \eval{ex1}
\end{run}

\subsubsection{Foldable}

It is possible to accumulate all values of a |Temporal| by providing a
function, like |+| or |*| and a starting value. Such things are called
|Foldable|.

\begin{code}
instance F.Foldable Temporal where
        foldr f z tpr = let (v,t, mtp) = step tpr
                            tpr'       = fromJust mtp
                        in case mtp of
                               Nothing -> f v z
                               _       -> f v (F.foldr f z tpr') 
\end{code}

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
        fmap f tpr = Temporal nxt
                where
                    (v,t, mtp) = step tpr
                    tpr' = do
                              tpx <- mtp
                              return (fmap f tpx)
                    nxt  = (f v, t, tpr')
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

\begin{code}
instance Applicative Temporal where
        pure x = fromList [(DPast, x)]
        f <*> x = let (vf,tf, mtpf)  = step f
                      (vx,tx, mtpx)  = step x
                      f'             = fromJust mtpf
                      x'             = fromJust mtpx
                      result remains = Temporal (vf vx, max tx tf, remains)          
                  in case cmpTnext mtpf mtpx of
                         Nothing -> result Nothing
                         Just LT -> result $ Just (f' <*> x)
                         Just GT -> result $ Just (f  <*> x')
                         Just EQ -> result $ Just (f' <*> x')
-- helper: compare the times of next changes
cmpTnext :: Maybe (Temporal a) -> Maybe (Temporal b) -> Maybe Ordering
cmpTnext Nothing      Nothing     = Nothing
cmpTnext Nothing      (Just tpr2) = Just GT
cmpTnext (Just tpr2)  Nothing     = Just LT
cmpTnext (Just tpr1)  (Just tpr2) = let (_,t1, _)  = step tpr1
                                        (_,t2, _)  = step tpr2
                                    in Just $ compare t1 t2

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


\end{document}

(define-key outline-minor-mode-map [(C-left)] 'hide-sublevels) 
