\documentclass[a4paper]{article}
%include lhs2TeX.fmt
%include greek.fmt
%options ghci -fglasgow-exts


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

% ------------------------------------------------------------
\section{Resources}
% ------------------------------------------------------------

The main property of a resource is its ability to be
\emph{consumed}. This happens as a response to a Resource-Request.

In earlier discussions we thought it would be good idea to distinguish
between \emph{mobile} and \emph{immobile} resources. In the |Siamos|
system this distinction is made. The system knows about each and every
immobile resource, but for the mobile resources it only knows their
number.

I don't think, that this is a good way to model things. First, whether
or not the system knows each resource "by name" is not a consquence of
that resource being immobile (though the two often coincide). So
immobile resources should rather be called \emph{enumerable},
indicating the fact that you can list them all, regardles of their
mobility. The other resources, i.e. the ones you only know by
quantity, should be called \emph{quantifiable} or something similar.

Second, whether or not you consider a resource enumerable or
quantifiable is not an attribute of the resource as such. Even you you
reason about "2 workers", the workers themselves all have names. The
fact that you don't bother about individual workers is an attribute of
the resource pool (it knows it has n workers, but it doesn't know
their names) and of the resource requests ("give me 2 workers" as
opposed to "give me Jim and Mary").

So we have

\begin{description}
\item[enumberable] resources, known by name
\item[quantifiable] resources, whose quantity is known
\end{description}

Furthermore a resource is described by "what it can do". Many
resources (particularly people) are able to several things. So the
capabilities of a resource is desctribed by a |Skill-set|.




% ------------------------------------------------------------
\subsection{Resouce Definition}

Before we get to defining Resources and Requests, we must firs define a Skill-Set

\begin{code}
import qualified Data.List as L
import qualified Data.Set as S
import qualified Text.Show.Pretty as Pr
import Data.Ord

data SkillSet = Skills (S.Set String) deriving Show
includesSkills (Skills a) (Skills b) = b `S.isSubsetOf` a
\end{code}

We define two types of Resources
\begin{description}
\item [Ers] for enumerable resources
\item [Qrs] for quantifiable resources
\end{description}

In order to do some reasonable algebra with these, we additionally
define a representation for "no resource".

\begin{code}
data Resource  = Ers String SkillSet | Qrs Int SkillSet | NoResource
               deriving (Show)

\end{code}

% ------------------------------------------------------------
\subsection{Request Definition}

Requests are very similar to Resources. In fact they are "Resources
you want to have".

\begin{code}
data Request   = Erq String SkillSet | Qrq Int SkillSet | NoRequest
               deriving (Show)
\end{code}

% ------------------------------------------------------------
\subsection{Operations with Resources and Requests}

Since both Requests and Resources know about Skills, we want
operations which answer whether a resouce satisfies the Skill-demand
of a request. Hence we create an abstraction for "things which know
about Skills".

\begin{code}
class Skilled a where
  skills     :: a -> SkillSet
  satisfies  :: (Skilled b) => a -> b -> Bool
  satisfies  a b =  (skills a)  `includesSkills` (skills b)
\end{code}

Resources and Request should now be instances of that class. If we
implement the |skills| function, we get |satisfies| for free.

\begin{code}
instance Skilled Resource where
  skills NoResource     = Skills (S.empty)
  skills (Ers _ skills) = skills
  skills (Qrs _ skills) = skills

instance Skilled Request where
  skills NoRequest      = Skills (S.empty)
  skills (Erq _ skills) = skills
  skills (Qrq _ skills) = skills
\end{code}

The quantifiable things have some resemblance with numbers in the
sense that you can reduce (and increase) the count. But the enumerable
things also behave like numbers, except their number is always zero or
one.

Again we create an abstraction for this quality

\begin{code}
class Countable a where
  count :: a -> Int
  minus :: a -> Int -> a
  cmin  :: (Countable b) => a -> b -> Int
  cmin x y = min (count x) (count y)
\end{code}

And make Resources and Requests instances of that class

\begin{code}
instance Countable Resource where
  count NoResource       = 0
  count (Ers _ _)        = 1
  count (Qrs c _)        = c
  
  minus res 0            = res
  minus rs@(Ers _ _) 1   = NoResource
  minus (Qrs c s) n
        | c > n          = Qrs (c-n) s
        | c == n         = NoResource
\end{code}

\begin{code}
instance Countable Request where
  count NoRequest        = 0
  count (Erq _ _)        = 1
  count (Qrq c _)        = c
  
  minus req 0            = req
  minus rq@(Erq _ _) 1   = NoRequest
  minus (Qrq c s) n
        | c > n          = Qrq (c-n) s
        | c == n         = NoRequest
\end{code}

This allows us to do algebra on the "count" property of qunatifiable
things. However, sometimes one of the operands may be a simple |Int|
(and not a |Resource| or a |Request|. In order to avoid special
treatments, we declare than an |Int| is a |Countable| too.

\begin{code}
instance Countable Int where
  count x    = x
  minus x y  = x - y
\end{code}

After these preparational steps it becomes very easy to define a
|satisfy| operation, which takes a Resource (af any kind) and a
Request (of any kind) and returns the remaining resource and the
remaining (unsatisfied) request.

\begin{code}
satisfy NoResource rq = (NoResource, rq)
satisfy res NoRequest = (res, NoRequest)

satisfy rs@(Ers rsName rsSkills) rq@(Erq rqName  rqSkills)
  | rsName == rqName  = (NoResource, NoRequest)
  | otherwise         = (rs, rq)

satisfy resource request
  | resource `satisfies` request  = (resource `minus` x, request `minus` x)
  | otherwise                     = (resource, request)
  where
    x = cmin resource request
\end{code}

% ------------------------------------------------------------
\section{Pools}
% ------------------------------------------------------------

The operations above only operate on a single Resouce and a single
Request. In the real world we typically want to satisfy a Request from
a |ResourcePool|.

\begin{code}
type ResourcePool = [Resource]
\end{code}

The Pool will contain both enumerable and quantifiable resouces and
there may be candidates of both types for satisfiying a
Request. Furthermore there may be resources which are
"overqwualified", i.e. they can do more things than we asked for. To
satisfy a request we must order our Resources in a particular way, so
we allocate the cheapest resources.

The following strategies seem sensible:

\begin{itemize}

\item Enumberable Requsts should be satisfied by enumerable Resources
and only if that isn't possible, quantifiable Resources shoud be
considered. We do not want the get Mary Wheeny, when we explicitly
aked for John Stout and John is available.

\item Quantifiable Requests should be satisfied by quantifiable
Resources. This is because these are less valuable, than enumerable
Resouces. Later on someone might explicitly ask for a certain
enumerbale resouce and may not get it, because we took it already.

\item We should always consider Resource with the least value first
and avoid overqualified resources.

\end{itemize}

So we need a number of sorting strategies. In fact all we need is a
number of comparison operators.

\begin{code}
type ResourceComparison = Resource -> Resource -> Ordering

-- $Qrs < Ers$
rsTypeOrder :: ResourceComparison
rsTypeOrder r1@(Qrs _ _) r2@(Qrs _ _) = EQ
rsTypeOrder r1@(Ers _ _) r2@(Ers _ _) = EQ
rsTypeOrder r1@(Qrs _ _) r2@(Ers _ _) = LT
rsTypeOrder r1@(Ers _ _) r2@(Qrs _ _) = GT
\end{code}

To sort by "skill value" we need to associate a value with a
skill-set. For now we will just count the skills, such that resources
with fewer skills will be considered first.

\begin{code}
rsSkillOrder :: ResourceComparison

rsSkillOrder r1 r2 = let (Skills s1) = skills r1
                         (Skills s2) = skills r2
                     in
                      compare (S.size s1) (S.size s2)
\end{code}

Now if we want so sort by multiple criteria, we need to combine
several such functions into one. Also we need to reverse the ordering.

\begin{code}
multiple :: ResourceComparison -> ResourceComparison -> ResourceComparison
multiple cmp2 cmp1 a b
  | cmp1 a b == EQ  = cmp2 a b
  | otherwise       = cmp1 a b

down :: ResourceComparison -> ResourceComparison
down cmp a b = case cmp a b of
  LT -> GT
  GT -> LT
  EQ -> EQ
\end{code}

and the two required ordering function are

\begin{code}
enumFirst = multiple rsSkillOrder (down rsTypeOrder)
quanFirst = multiple rsSkillOrder rsTypeOrder
\end{code}

Satifying a request from a Pool is now a matter of properly sorting
the Pool and doing a little List processing. Unfortunately the order
of parameters in our function does not work well with |MapAccumL|, so
we need to doe some swapping and flipping.

\begin{code}
poolSatisfy :: ResourcePool -> Request -> (ResourcePool, Request)
poolSatisfy pool req = swap $ L.mapAccumL satisfy' req sortedPool
  where
    swap (x,y) = (y,x)
    satisfy' a b = swap $ satisfy b a
    sortedPool = case req of
      (Qrq _ _) -> L.sortBy quanFirst pool
      (Erq _ _) -> L.sortBy enumFirst pool
\end{code}



% ------------------------------------------------------------
\subsection{Example Pool}

Let's define some convenience functions to define resources and requests.

\begin{code}
qrs :: Int -> [String] -> Resource
qrs n skills = Qrs n (Skills $ S.fromList skills)

ers :: String -> [String] -> Resource
ers n skills = Ers n (Skills $ S.fromList skills)

qrq :: Int -> [String] -> Request
qrq n skills = Qrq n (Skills $ S.fromList skills)

erq :: String -> [String] -> Request
erq n skills = Erq n (Skills $ S.fromList skills)


\end{code}

To get a better overview of the result, we count the resources by type

\begin{code}
summarizePool :: ResourcePool -> (Int,Int)
summarizePool pool = (qrss, erss)
  where
    qrss = L.sum $ map qrsCount pool
    qrsCount r = case r of
      (Qrs c _) -> c
      otherwise -> 0
    erss = length $ filter isErs pool
    isErs r = case r of
      (Ers _ _) -> True
      otherwise -> False

summarize (pool, req) = (summarizePool pool, req)

erss = 1
\end{code}

\begin{code}
examplePool = [
  qrs 16            ["sweep", "feed"],
  qrs 10            ["driveForklift","unload", "load"],
  qrs 2             ["unload", "load"],
  qrs 4             ["runProg100", "runProg101", "runProg102"],
  qrs 2             ["runProg200", "runProg201"],
  ers "John Stout"  ["unload", "load"],
  ers "Irv1"        ["runProg200"],
  ers "Irv2"        ["runProg201"]
  ]
\end{code}

\end{document}
