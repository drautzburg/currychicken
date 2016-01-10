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
\newmdenv[frametitle=Example,backgroundcolor=gray!05,roundcorner=2pt]{run}

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
\title{Constructing Sorting Products}
\begin{document} \maketitle 

\begin{abstract}

\emph{Sorting Products} are a way to express what mail a process
\emph{accepts}. In the case of lettermail, they originate at the
reciepients' mailboxes, which accept all mail for any of the
recipients listed on it.

The various processess in mail processing transform Sorting Products
and ultimately create the Products which are sold by the postal
organisation, e.g. "drop a letter in a letterbox and we'll bring it
anywhere".

This paper attempts to formalize these transformations.


\end{abstract}


\tableofcontents 
\listoffigures
%if false
\begin{code}
import qualified Data.List as L
import qualified Data.Set as S
import Test.QuickCheck
import Data.Maybe
import Text.Show.Pretty
import Control.Arrow
-- import Data.String.Utils
pp a = putStrLn $ ppShow a
lpp a = do
    putStrLn "\\begin{verbatim}"
    putStrLn $ ppShow a
    putStrLn "\\end{verbatim}"

\end{code}
%endif

\section{Items and Products}

\subsection{Items}

Mail items can be letters, parcels, trays, roll-containers, truck and
many more. It appears like some of them are containers and others are
\emph{atomic}. However emphasizing the distinction between containers
and atomic items turned out to be suboptimal, primarily because this
distinction lies in the eye of the beholder and is not a true property
of a mail item.

For a parcel processing company a parcel appears like an atomic
item. However it is certainly a container and it does contain other
items. It only appears atomic, because the parcel company does not
care about what's inside,

Therefore we treat all items as containers which could potentially
contain other items. We need an additional means of expression for
items where we don't care (or know) what's inside.

Furthermore items shall carry a label. The label stands for everything
we know about the item. The weight of a parcel may technically not be
printed on a label, but we treat it as part of the label
nontheless. We make no assumption about the label format, i.e. whether
an item is characterized by Format, Class and Destination or by
anything else.

So an item with a certain type of Label |lty| is either
\begin{description}
\item[a nested item], i.e. one where we know what's inside. Such an
item is characterized by its own label and the items contained in it.
\item[a nonempty item], i.e. one where we don't know what's inside and
which is characterized by its own label only.
\end{description}

The \emph{nonempty} items are in a way the \emph{atomic} items we
talked about earlier. There is a clear distinction between such
nonempty/atomic items and an empty container. We known precisely
what's inside an empty container, namely nothing.

These consideration lead to the following definition:
\begin{code}
data Item lty = Inest lty [Item lty] | 
                Inonempty lty
              deriving (Eq, Ord, Show)
\end{code}

Note that the label type |lty| must be general enough to describe all
labels in our universe. We cannot e.g. define seperate label types for
trays and letters. In practice, this is not a limitation, because
different label types can always be united under one union-type.

\subsection{Products}

A Product tells us, whether or not it \emph{accepts} a given item.
Hence, at its heart, a Product is a \emph{predicate}, i.e. a function
|Item lty -> Bool|.

\needspace{12em} This leads to the following typeclass (think:
interface): There is a function |accepts| which takes a
Product and an Item and answers either |True| or |False|.

Products and Items must agree on the label type |lty|. It must be
possible to compare labels for equality (|Eq|) and to order them
(|Ord|).

\begin{code}
class Product prod where
        accepts :: (Eq lty, Ord lty) => 
                   prod lty -> Item lty -> Bool

\end{code}

As we shall see, this definition alone does not buy us much. We still
have to find a concrete implementation of that typeclass/interface.

\subsection{Products for Items with negative value}

When you buy a commercial Product, e.g. an Amazon Firestick, you
expect to receive the Firestick itself, a remote control a power
supply, two batteries and an HDMI extender cable. If any of these
items are \emph{missing}, you have reason to complain and you may
refuse to accept the item. You would however, not complain when there
were some extra items in the package, like some flyers or candy. This
is because all these items have a value greater than zero.

When dealing with mail, things are different. Mail items have a value
less than zero. A process in the chain will not complain when it
receives nothing at all. It only complains, when it receives something
it doesn't want, just like you, as a mail recipient, will
complain when you receive mail which is not for you.

So Sorting Products do not specify what must be included, but what
\emph{may} be included. Or alterternatively, they specify what must
\emph{not} be included.

\section{Processes}

We shall now look at the various processes in the postal world and
examine how they transform Sorting Products. 

There are six elementary Processes, namely \emph{Merge, Split, Pack,
Unpack, Buffer} and \emph{Transport}. Only the first four transform
Products, while the last two leave them unchanged, at least when you
assume that Products are defined irrespective of Time and
Place\footnote{It would be interesting to see, where we end up when we
make Time and Place part of Sorting Products}.

You may be tempted to believe that these Procersses can be combined in
an arbitrary way. But this is not the case. Also, there are cases
where it is not possible to compute input Products from output
Products alone.

We'll use diagrams to illustrate the processes.  The arrows in those
diagrams point in the direction of the Item flow. The transformation
of Sorting Products however, runs in the opposite direction, i.e. the
input Products are computed from the output products. To emphasize
this, we label the input products with |y|, as they are the result of
the transformation and the output products with |x|, as they are the
input to the transformation. When we need to distinguish between items
and containers, We use the letter |i| to refer to items and |c| for
containers.

\subsection{Pack}

 \begin{figure}[htb!]
\centering
\includegraphics[width=5cm]{ProductsPack.eps}
\caption{Pack}
\end{figure}

Pack takes Items and places them inside a Container with a specified
label |yc|. 

You can compute |yc| and |yi| only when |xi| accepts containers whith
a single label. Unfortunately, there are Products which reference more
than one container label (see |Split| below).


\subsection{Unpack}

 \begin{figure}[htb!]
\centering
% \includegraphics[width=5cm]{ProductsUnpack.eps}
\caption{Unpack}
\end{figure}

To compute the Product |yi|, we must know the container label |xc|. It
cannot be computed from anything, but needs to be specieifed by the
planner. However once set, this information travels "left" through
other processes, such that a potential upsream |Pack| Process will
know what label to use.

\subsection{Merge}
 \begin{figure}[htb!]
\centering
\includegraphics[width=5cm]{ProductsMerge.eps}
\caption{Merge}
\end{figure}

When given an output Product |xi| it is not possible to compute the
input products |yi1| and |yi2| without further information. First you
wouldn't know how many inputs there are and second, just accepting
|xi| at any input would be a perfectly valid solution.

\begin{figure}[htb!]
\centering
\includegraphics[width=5cm]{ProductsMerge2.eps}
\caption{Merge as Intersections}
\end{figure}

The least thing you need to specify, is what |p| you accept from each
input. The resulting |yi| Products can then be computed, such
that each item that is accepted, is accepted both by the respective
|p| and |xi|, i.e.

\begin{eqnarray}
accepts(p_n, item) \wedge accepts(xi, item) \Leftrightarrow accepts (yi_n, item)
\end{eqnarray}

There is no guarantee, that the |yi| Products together accept all mail
that is accepted by |xi|. However this is not a problem, because |xi|
only tells you what you \emph{could} send to the next Process, it
doesn't mean you have to.

\subsection{Split}
\begin{figure}[htb!]
\centering
\includegraphics[width=5cm]{ProductsSplit.eps}
\caption{Merge}
\end{figure}

A Split accepts any item, which is accepted by one its outputs $xi_n$.

\begin{eqnarray}
\bigvee_n accepts(xi_n, item) \Leftrightarrow accepts(yi,item) 
\end{eqnarray}

But the Split processes we encounter in the real world only look at
the containers and do not make decisions based on what's inside a
container. This has the consequence that a Split can never be placed
after a Pack.

\begin{figure}[htb!]
\centering
\includegraphics[width=7cm]{ProductsSplitPack.eps}
\caption{No Split after Pack}
\end{figure}

The Pack process will pack all intems into containers with the same
label, so the Split Process has no way of distinguishing those
containers from each other. 

Likewise the |Pack| process has no way of figuring out the required
container label |yc|, because the |Split| process accepts a |Pfunc|,
i.e. a union of things, with multiple possible container labels.

\section{Product Representations}

\subsection{General considerations}

The typeclass |Product| defines the behavior of Products in a somewhat
abstract fashion. We still need to find a suitable
implementation. From this implementation we demand, that is must be
possible to convert a Product into a String. Otherwise we would not be
able to see the results of our transformations.

The |Product| typeclass is basically a function, and in general you
cannot convert a function to a String. You can however, implement a
function as a table, where the columns hold every possible parameter
combination and one final column holds the function's return value.

Since our function has a boolean return value, we do not have to list
all parameter combinations, but just the ones whose return value is
|True|. If a parameter combination is not found in the table, the
result must be |False|.

There is one catch though: as Items can be arbitrarily nested,
Products need to be able to express arbitrary nesting as well. To
encode this in a table, we would need an infinite number of columns.

However, arbitrary nesting as such is not a problem at all. Any
tree-like data structure is able to express that. It is the
combiniation of encoding a function and encoding arbitrary nesting,
which makes things slightly more difficult.

\subsection{Concrete Implementation}

The above considerations about nesting and about |Split| not being
allowed after |Pack| suggest, that a Product-representation needs two
parts: one describes a container with its possible conents (encoding
of nesting), the other one describes the combinations, which return
|True| (encoding of function). These two mutually reference each
other.

This leads to the following:

\begin{code}
data Pfunc lty    = Pfunc [Pnest lty] | PfAny | PfNone
                  deriving (Eq, Ord, Show)
data Pnest lty   = Pnest lty (Pfunc lty)
                  deriving (Eq, Ord, Show)
\end{code}

A |Pfunc| is basically just a list of |Pnests|, with the two
additional constructors |PfAny| and |PfNone|, which match anything or
nothing respectively.

A |Pnest| is defined by a container label of type |lty| and the
possible content of the container, which is defined as a |Pfunc|.

Note that
\begin{itemize}
\item A |Pfunc| (or |PfAny| or |PfNone|) is always followed by a |Pnest| and vice versa.
\item Expect to always find a |PfAny| or an empty list |[]| at the end.
\end{itemize}

A |Product| is then a union-type of these two parts
\begin{code}
data ProductRep lty = FuncRep (Pfunc lty) | 
                      NestRep (Pnest lty)
\end{code}


Let's define a |Pfunc| with toplevel Labels "foo" and "bar", where
"foo" may contain "foo1" and "foo2"-labeled items and "bar" may
contain nothing at all. 

\begin{code}
ex_foo =  Pnest "foo" 
          (Pfunc [
            Pnest "foo1" PfAny,
            Pnest "foo2" PfAny
           ])

ex_bar =  Pnest "bar" (Pfunc [])

ex_pfunc1 = Pfunc [ex_foo, ex_bar]

-- The whole Product is a |FuncRep|.
ex_prod1 = FuncRep ex_pfunc1
\end{code}

\subsubsection{The Product instance}

We should now be able to define a |Product| instance of
|ProductRep|. Otherwise |ProductRep| would not be a suitable
implementation of |Product|. We begin by answering when a |Pnest| and a |Pfunc| accept an Item. 

A |Pfunc| accepts an Item, when it is accepted by one of its |Pnest|
elements. We test this, using a yet-to-be-defined function
|pAccepts|. Since our accept functions take the "Product" as the first
parameter and the Item as second, we have to flip the parameters in
order to apply |any|. Finally there are the cases for |PfAny| and
|PfNone| with obvious implementations.

\begin{code}
fAccepts :: (Ord lty) => Pfunc lty -> Item lty -> Bool
fAccepts (Pfunc pns) item = any (flip nAccepts $ item) pns
fAccepts PfAny _  = True
fAccepts PfNone _ = False
\end{code}

The implementation for |nAccepts| works as follows: when testing an
|Inonempty| we can only accept it, when the Product allows |PfAny| as
contained Items and the toplevel Labels match. If we test an
|Inest|, then also the toplevel Labels must match, but also |all|
contained items must be accepted. In all other cases, the Item is not
accepted.

\begin{code}
nAccepts :: (Ord lty) => Pnest lty -> Item lty -> Bool
nAccepts (Pnest plbl PfAny) (Inonempty ilbl)  = plbl == ilbl
nAccepts (Pnest plbl frep) (Inest ilbl items) = plbl == ilbl && 
                                                all (fAccepts frep) items
nAccepts _ _  = False

\end{code}

Now all the |Product| instance has to do, is dispatch to either
|pAccepts| or |nAccepts|.

\begin{code}
instance Product ProductRep where
        accepts (NestRep prod) item = nAccepts prod item
        accepts (FuncRep prod) item = fAccepts prod item
        
\end{code}

\subsubsection{Testing the Product instance}

Let's run some ad-hoc tests:\medskip

\begin{run}
We don't accept a nonempty "foo", because we only allow "foo1" or
"foo2" inside, but we don't know what's inside.

|*Main> accepts ex_prod1 (Inonempty "foo")|\\
  \eval{accepts ex_prod1 (Inonempty "foo")}

However, a "foo" with a "foo1" inside is accepted.

|*Main> accepts ex_prod1 (Inest "foo" [Inonempty "foo1"])|\\
  \eval{accepts ex_prod1 (Inest "foo" [Inonempty "foo1"])}
\end{run}
\medskip\needspace{12em}
\begin{run}
An empty "foo" is accepted too

|*Main> accepts ex_prod1 (Inest "foo" [])|\\
  \eval{accepts ex_prod1 (Inest "foo" [])}

And so is an empty "bar"

|*Main> accepts ex_prod1 (Inest "bar" [])|\\
  \eval{accepts ex_prod1 (Inest "bar" [])}
\end{run}

\needspace{12em}
\subsection{Set Operations}

Let's now define some Set-operations.

\subsubsection{Union}

The union of two |Pfuncs| can be computed by buiding the union of the
underlying lists. Other than that, there are some obvious corner cases
concerning |Pany| and |Pnone|\footnote{to DTZ: Pfunc is not a Monoid, as it required Ord.}.

\begin{code}
fUnion :: Ord a => Pfunc a -> Pfunc a -> Pfunc a
fUnion PfAny _  = PfAny
fUnion _ PfAny  = PfAny
fUnion PfNone x = x
fUnion x PfNone = x
fUnion (Pfunc as) (Pfunc bs) = Pfunc (L.union as bs)
\end{code}


\subsubsection{Intersection}

Two |pNests| can be intersected, which may or may not produce a
result. Disjoint |pNests| will produce Nothing. The operation calls
|fIntersection| to intersect the possible contained items.

\begin{code}
nIntersection :: (Ord a) => Pnest a -> Pnest a -> Maybe (Pnest a)
nIntersection (Pnest a as) (Pnest b bs)
  | a == b    = Just $ Pnest a (fIntersection as bs) 
  | otherwise = Nothing
\end{code}
\medskip
\begin{run}

Intersecting disjoint

|*Main> nIntersection ex_foo ex_bar|\\
  \eval{nIntersection ex_foo ex_bar}

Intersection with oneself

|*Main> nIntersection ex_bar ex_bar|\\
  \eval{nIntersection ex_bar ex_bar}

\end{run}

\needspace{12em}
One can filter a |Pfunc| with a |pNest| such that only those list
items prevail, which are part of the |pNest|.

\begin{code}
fFilter :: (Ord a) => Pnest a -> Pfunc a -> Pfunc a
fFilter pn PfAny = Pfunc [pn]
fFilter _ PfNone  = PfNone
fFilter pn (Pfunc pns) = Pfunc $ foldr f [] pns
  where
    f pn' ys = case nIntersection pn pn' of
      (Just y') -> y':ys
      otherwise -> ys
\end{code}

\needspace{12em}
\begin{run} 
We can filter our |ex_pfunc1| such that only "foo1" inside a "foo" are
allowed. No more "bar" toplevels and no more "foo2" inside a "foo" are
accepted.

|*Main> fFilter (Pnest "foo" (Pfunc [Pnest "foo1" PfAny])) ex_pfunc1|
  \eval{fFilter (Pnest "foo" (Pfunc [Pnest "foo1" PfAny])) ex_pfunc1}
\end{run}


\needspace{12em}
Finally the intersection of |Pfunc|. Basically we build the union of
filtering the second |Pfunc| by every |pNest| in the first |Pfunc|.

\begin{code}
fIntersection :: Ord a => Pfunc a -> Pfunc a -> Pfunc a
fIntersection PfAny x  = x
fIntersection PfNone x = PfNone
fIntersection x PfAny  = x
fIntersection x PfNone = PfNone
fIntersection (Pfunc pcks1) pls = foldr fUnion (Pfunc []) $ do
  pck1 <- pcks1
  return $ fFilter pck1 pls
\end{code}


\subsection{Processes transforming Products}

Because we destinguished between |Pfunc| and |Pnest| we can now be
specific about what kind of Product each Process transforms and what
the type of the transformed Product is. It will not be possible to
accidently place a |Split| afer a |Pack|, because (as we shall see)
|Split| computes a |Pfunc|, which is not transformed by a |Pack|.


\subsubsection{Split}

|Split| takes a number of |Pfuncs|, which stand for the Products which
are accepted e.g. by the Stackers, and computes the Product which is
accepted by the feeder of the machine. The computed Product is again a
|Pfunc|.

\begin{code}
split :: (Ord a)=> [Pfunc a] -> Pfunc a
split pxs = foldr fUnion (Pfunc []) pxs
\end{code}

\subsubsection{Merge}

|Merge| does the inverse operation, but as stated earlier, it needs
additional information to decide, what to accept at each input. This
additional information comes in the form of a list of |Pfunc|, one
for each input. The result of the computation is a list of |Pfunc|,
again one for each input.

\begin{code}
merge :: (Eq a, Ord a) => [Pfunc a] -> Pfunc a -> [Pfunc a]
merge plss pls = map f plss
  where
    f p = fIntersection p pls

-- xxx
mergeAll :: (Eq a, Ord a) => Pfunc a ->[Pnest a]
mergeAll (Pfunc pns) = pns

\end{code}

\subsubsection{Pack} |Pack| takes a |Pnest| and computes the
container label and the |Pfunc| for the items it accepts. Essentially
it removes one level of nesting.

\begin{code}
pack :: Pnest lty -> (lty, Pfunc lty)
pack (Pnest lbl pls) = (lbl, pls)
\end{code}

\subsubsection{Unpack}
|Unpack| takes a container label and a |Pfunc| and produces a |Pnest|.


\begin{code}
unpack :: lty -> Pfunc lty -> Pnest lty
unpack lbl pls = Pnest lbl pls
\end{code}

\section{Larger examples}

 \begin{figure}[htb!]
\centering
\includegraphics[width=12cm]{ProductsExRec.eps}
\caption{Pack}
\end{figure}

\begin{code}
leaf :: lty -> Pfunc lty
leaf lbl = Pfunc[Pnest lbl PfAny]

splitAll :: Ord a => [Pnest a] -> Pfunc a
splitAll ns = split $ map (\np -> Pfunc [np]) ns
\end{code}

\begin{code}
type Ex_lbl = (String, String, String)
ex_truck = 
        let 
           --shorthands
           lbl s x y = s ++ (show x) ++ (show y)
           clbl s1 s2 n = (s1, s2 ++ (show n), "")
           range size n = [size*n .. size*(n+1)-1]
           ------------------------------------------------------------
           -- Receiver side
           ------------------------------------------------------------
           -- The nth route consists of the following addresses
           -- xxx split ord and prio
           rRoute route = split [leaf("Letter", lbl "Addr" route i, mclass)
                                         | i <-[1..10],
                                               mclass <- ["Ord", "Prio"]
                               ] :: Pfunc Ex_lbl
           -- Each route has its own |Pnest| tray product
           rTray route   = unpack (clbl "Tray" "Route" route) (rRoute route)      
                         :: Pnest Ex_lbl

           -- Each delivery office is responsible for 10 routes ...
           rDof dof    = splitAll [rTray route | route <- range 10 dof] 
                       :: Pfunc Ex_lbl

           -- ... and has a dedicated rollcontainer
           rRc n     = unpack (clbl "RollContainer" "DO" n) (rDof n)  
                     :: Pnest Ex_lbl

           -- a Region services 5 Delivery Offices ...
           rRegion reg = splitAll [rRc i | i <- range 5 reg]     
                       :: Pfunc Ex_lbl

           -- ... and has a truck bringing the rollcontainers
           rTruck n  = unpack (clbl "Truck" "Region" n) (rRegion n)   :: Pnest Ex_lbl

           ------------------------------------------------------------
           -- Now what happens at the departure side of the truck
           ------------------------------------------------------------
           -- The truck to region n gets packed
           sTruck n = pack(rTruck n)               :: (Ex_lbl, Pfunc Ex_lbl)

           -- Each rollcontainer comes from a different source ...
           sRcs reg  = mergeAll (snd (sTruck reg))     :: [(Pnest Ex_lbl)]

           -- ... where they get packed
           sDof reg dof  = pack (sRcs reg !! dof)          :: (Ex_lbl, Pfunc Ex_lbl)

           -- 

           in (sRcs 0) -- (stray 0) !! 0
\end{code}

%\begin{figure}[htb!]
%\centering
%\includegraphics[width=4cm]{glass-slipper.jpg}
%\end{figure}

\end{document}
