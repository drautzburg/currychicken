\documentclass{article}
%include lhs2TeX.fmt
%include lhs2TeX.sty
%format <-- = "\boldmath{${\leftarrow}$}"
%format <- = "\char''30"
%options ghci
\usepackage{fancyhdr}
\pagestyle{fancy}
\usepackage{ragged2e}
\usepackage[parfill]{parskip}
\usepackage{pgf}
\usepackage{needspace}
\usepackage[framemethod=tikz]{mdframed}
\newmdenv[frametitle=Example,backgroundcolor=blue!05,roundcorner=2pt]{example}
\newmdenv[frametitle=Running it,backgroundcolor=gray!05,roundcorner=2pt]{run}
\usepackage[utf8]{inputenc}
\newlength{\currentparskip}

\author{Martin Drautzburg}

\title{Temporal Data}

\begin{document} \maketitle \tableofcontents 


\section{Behavior}

\begin{code}
import Data.List
import Data.List.Ordered
import Debug.Trace
import Text.Printf

type Day = Int
\end{code}


A |Behavior| is a value which can change from day to day. When the day
is known, we can ask for the value which is valid on that day. So a
|Behavior| is a function from |Day| to something.

\begin{code}
class Behavior b where
        at :: (Eq a) => b a -> Day -> a

\end{code}

We can also scan a number of days, and we expect a list of days, each
one associcated with a value.

\begin{code}
between :: (Eq v, Behavior b) => Day -> Day -> b v -> [(Day, v)]
between d1 d2 bhvr = zip [d1..d2] (map (at bhvr) [d1 .. d2])
\end{code}

Or we can ask for seven days in a row. While this is a full week, it
does not have to start on a Monday or any other specific weekday.

\begin{code}
weekFollowing :: (Eq v, Behavior b) => Day -> b v ->  [(Day, v)]
weekFollowing day bhvr = between day (day+6) bhvr
\end{code}

\subsection{Change}

|Change| is a data structure, which is useful for implementing
|Behavior|. It is a default value plus a number of changes, ordered by
day.

\begin{code}
type (Change v) = (v, [(Day,v)])
\end{code}

To get the value of a particular day, we reason as follows: since we
know, that the list of changes is ordered, we need to take the default
if the first day in the list is already later that the day we're
asking for. Otherwise, we make the value of the first day the default
and continue with the rest of the list.

\begin{code}
valueAt :: (Change v) -> Day -> v
valueAt (def,[]) _ = def
valueAt (def, ((d,v):cs)) day 
           | d > day   = def
           | otherwise = valueAt (v, cs) day
\end{code}

The list of changes must be kept ordered by day. When we insert a new
change, we better make sure this ordering is preserved. Also, if we
insert a change, where there was already a change, we replace the
change, i.e. in case of a conflict that change which was inserted last
wins.


\begin{code}
insertChange :: Day -> v -> (Change v) -> (Change v)
insertChange  d x (def, chgs) = (def, chgs')
        where
            clean = filter (\(dx,_) -> dx /= d) chgs 
            chgs' = insertBagBy dayOrder (d,x) clean
            dayOrder (d1,_) (d2,_) = compare d1 d2
\end{code}

A complex |Change| is typically constructed by a number of
|insertChange| calls and the initial |Change| is just the default
value. To make that look prettier, we define this function:

\begin{code}
normally a = (a, []) 
\end{code}

\subsection{Displace}

An obvious application of |Change| is a value which changes at
particular days. We call this |Displace|.

\begin{code}
data Displace v = Displace (Change v) deriving (Eq, Show)
\end{code}

 To make |Displace| and instance of |Behavior| we must implement the
|at| function. There is not much to do, because |Change| already
brings most of the functionality. All it takes is a little wrapping.

\begin{code}
instance Behavior Displace where
        at (Displace chg) day = valueAt chg day
\end{code}

We will use |String|s as values for demonstration purposes. In reality
values can have any type.

\needspace{20\baselineskip}
\begin{example}
\begin{code}
exampleDisplace = Displace $ 
    (insertChange 2 "two")   $
    (insertChange 8 "eight") $ 
    (insertChange 4 "four")  $
    (insertChange 10 "ten") (normally "")               
\end{code}
\end{example}

\needspace{20\baselineskip}
\begin{run}
|*Main> exampleDisplace|\\
  \eval{exampleDisplace}


|*Main> between 1 5 exampleDisplace|\\
  \eval{between 1 5 exampleDisplace}

|*Main> at exampleDisplace 3|\\
  \eval{at exampleDisplace 3}

\end{run}

\subsection{Repeat}

|Repeat| is an alternative way of specifying a |Behavior|. While
|Displace| expresses things which change at a certain day, |Repeat|
expresses a |Behavior|, which changes depending on the weekday and
which repeats every week.

Using pattens like |MTW----| is attractive, because we can cover three
(or more) days in a concice way. However it bares the risk of
overlapping patterns. If |M-W-F-S| is associated with one value and
|MT-T-S-| with another, what value do we expect on Monday?

|Change| does not suffer from this problem. In fact we can derive
|Repeat| from |Change| in much the same way in which we derived
|Deisplace| from |Change|. The only difference is that we must take the
day-of-the-week instead of the day itself, when we ask for a value
using |at|. And of course there is no point in specifying days larger
than 6 in the list of changes -- they would have no effect.

\begin{code}

data Repeat v = Repeat (Change v)
                        deriving (Eq)

instance Behavior Repeat where
        at (Repeat chg) day = valueAt chg (day `mod` 7)

\end{code}

We may however, wish to present |Repeats| as a list of
|(Value,Pattern)|. This apparently simple transformation requires a
lot of code (by Haskell standards). This may indicate, that this kind
of representation bares some hidden complexity.

\begin{code}
asPatterns :: (Ord v, Show v) => [(Day, v)] -> [(v, String)]
asPatterns betweens = map combine $ group $ betweens
        where
            group xs  = let val cmp l1 l2 = cmp (snd l1) (snd l2)
                        in groupBy (val (==)) $ 
                           sortBy (val compare) xs
            combine xs = let value = snd $ head xs
                             pattern = foldr f "-------" xs
                             f (d,v) pat 
                                     = let d' = d `mod` 7
                                       in
                                           (take d' pat) 
                                           ++ ["MTWTFSS" !! d' ] 
                                           ++ (drop (d'+1) pat)
                         in (value, pattern)
-- use |asPattern| when printing |Repeat|
instance (Ord v, Show v) => Show (Repeat v) where
       show x = "Repeat " ++ (show $ asPatterns $ between 0 6 x)

\end{code}

\needspace{10\baselineskip}
\begin{example}
\begin{code}
exampleRepeat = Repeat $
     (insertChange 0 "Even")  $
     (insertChange 5 "End") $ 
     (insertChange 2 "Even")  $ 
     (insertChange 1 "Odd")  (normally "") 
\end{code}
\end{example}

\needspace{20\baselineskip}
\begin{run}
|*Main> exampleRepeat|\\
  \eval{exampleRepeat}


|*Main> at exampleRepeat 0|\\
  \eval{at exampleRepeat 0}

|*Main> at exampleRepeat 1|\\
  \eval{at exampleRepeat 1}

|*Main> at exampleRepeat 2|\\
  \eval{at exampleRepeat 2}

|*Main> at exampleRepeat (2+7)|\\
  \eval{at exampleRepeat (2+7)}

\end{run}


\subsection{Evolution}

We chose the term |Evolution| to describe a |Behavior| which changes
depending on the day-of-the-week and additionally on certan days. This
is a typical thing in planning scenarios.

Conceptually this is a |Behavior| of a |Behavior|. The second (inner)
|Behavior| is a |Repeat| and gives us a value for every
day-of-the-week. The outer |Displace| describes how the inner
|Behavior| changes over time.

\begin{code}
data Evolution v = Evolution (Displace (Repeat v))
                 deriving (Eq,Show)

instance Behavior Evolution where
        at (Evolution displc) day = at repeat day
                where
                    repeat = at displc day
\end{code}

\needspace{20\baselineskip}
\begin{example}
\begin{code}
exampleEvolution = 
        let defaultWeek = Repeat $
                          (insertChange 0 "DefaultWkday")$
                          (insertChange 5 "DefaultWkend")  
                          (normally "") 
            winterWeek = Repeat $
                          (insertChange 0 "WinterEven")  $
                          (insertChange 5 "WinterWkend") $ 
                          (insertChange 2 "WinterEven")  $ 
                          (insertChange 1 "WinterOdd")  
                          (normally "") 
            summerWeek = Repeat $
                          (insertChange 0 "SummerEven")  $
                          (insertChange 5 "DefaultWkend")$ 
                          (insertChange 2 "SummerEven")  
                          (normally "") 
        in Evolution $ Displace $
                   (insertChange 40 winterWeek) $
                   (insertChange 80 summerWeek)
                   (normally defaultWeek)
\end{code}
\end{example}

\begin{run}
|*Main> between 39 41 exampleEvolution|\\
  \eval{between 39 41 exampleEvolution}

|*Main> asPatterns $ weekFollowing 35 exampleEvolution|\\
  \eval{asPatterns $ weekFollowing 35 exampleEvolution}
\end{run}

\subsection{Exception Days}

If we want to create an exception day, i.e. a day where something
happens out of the ordinary, we must replace the |Repeat| which is
valid at that day by another |Repeat|. Since we only want to change a
single day, there is no point in replacing it by a full-fleged
|Repeat|. We do not need to know what happens on every
day-of-the-week.

So, the |Repeat| which holds the exceptional value only needs a
default value. We cannot elegantly specify an exceptional
\emph{weekend} this way, but we are only providing a convenience
operation here. If more than one day changes, it is better to use the
standard functionality and specify a full alternative week.

\needspace{2\baselineskip}
For the day after the exception day, we must restore whatever |Repeat|
used to be valid then.

\begin{code}
exceptionDay :: Day -> v -> Evolution v -> Evolution v
exceptionDay  day val (Evolution (Displace chg)) = 
        let 
            nextRep = valueAt chg (day+1)
            xcptRep = (Repeat (normally val))
        in  Evolution $ Displace $
              (insertChange day (xcptRep))  $
              (insertChange (day+1) nextRep) 
              chg
\end{code}

\begin{run}

--- Look at the unchanged example: ---

|*Main> between 39 41 exampleEvolution|\\
  \eval{between 39 41 exampleEvolution }

--- Define two shortcuts to insert exception days ---

|*Main> let exc1 = exceptionDay 39 "Ex39"|\\
  \eval{let exc1 = exceptionDay 39 "Ex39"}
  \eval{:t exc1}

|*Main> let exc2 = exceptionDay 40 "Ex40"|\\
  \eval{let exc2 = exceptionDay 40 "Ex40"}
  \eval{:t exc2}

--- Apply the first one ---

|*Main> between 39 41 (exc1 exampleEvolution)|\\
  \eval{between 39 41 (exc1 exampleEvolution) }

--- Apply both ---

|*Main> between 39 41 (exc2 $ exc1 exampleEvolution) |\\
  \eval{between 39 41 (exc2 $ exc1 exampleEvolution) }

\end{run}

\section{Temporal Tables - unfinished}

When you say that a person changed, you assume that it still the same
person. When you notice a new building somewhere, you refer to the
same place before and after the building was there. In the real world
every change is associated with something that does not change. It
makes no sense to talk about change without saying "a change of
\emph{what}"

If we translate this to Entities, then we must realize, that a
temporal row makes little sense. Something must remain constant there
too.

Things look different for Relationships. A person can be married to
different other persons over time, and there is nothing constant in
the \emph{is-married-to} relationship. Conceptually a Releationship is
a function from tuple to |Bool|, which changes over time.

\begin{code}
data Table c t= Table [(c, Evolution t)]
tat :: Eq t => Table c t -> Day -> [(c, t)]
tat (Table rows) day = map (\(c,t) -> (c, at t day)) rows


exampleTemporal1 = 
        let defaultWeek = Repeat $
                          (insertChange 0 ("DefaultWkday",1,2)) $
                          (insertChange 5 ("DefaultWkend",1,3))  
                          (normally ("default",0,0)) 
            winterWeek = Repeat $
                          (insertChange 0 ("WinterEven",4,6))  $
                          (insertChange 1 ("WinterOdd",6,7)) 
                          (normally ("default",0,0)) 
            summerWeek = Repeat $
                          (insertChange 5 ("DefaultWkend",4,5))$ 
                          (insertChange 2 ("SummerEven",4,8))  
                          (normally ("default",0,0)) 
        in Evolution $ Displace $
                   (insertChange 40 winterWeek) $
                   (insertChange 80 summerWeek)
                   (normally defaultWeek)

exampleTemporal2 = 
        let defaultWeek = Repeat $
                          (insertChange 0 ("2DefaultWkday",1,2)) $
                          (insertChange 5 ("2DefaultWkend",1,3))  
                          (normally ("default",0,0)) 
            winterWeek = Repeat $
                          (insertChange 0 ("2WinterEven",4,6))  $
                          (insertChange 1 ("2WinterOdd",6,7)) 
                          (normally ("default",0,0)) 
            summerWeek = Repeat $
                          (insertChange 5 ("2DefaultWkend",4,5))$ 
                          (insertChange 2 ("2SummerEven",4,8))  
                          (normally ("default",0,0)) 
        in Evolution $ Displace $
                   (insertChange 20 winterWeek) $
                   (insertChange 30 summerWeek)
                   (normally defaultWeek)

exampleTable = Table [
                ("pk1", exampleTemporal1),
                ("pk2", exampleTemporal2)]


\end{code}


\begin{run}
|*Main> tat exampleTable 0|\\
  \eval{tat exampleTable 0}

|*Main> tat exampleTable 5|\\
  \eval{tat exampleTable 5}

|*Main> tat exampleTable 20|\\
  \eval{tat exampleTable 20}

|*Main> tat exampleTable 40|\\
  \eval{tat exampleTable 40}

\end{run}



\end{document}