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
recipients listed on it and is of the correct type
(e.g. non-registered mail).

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
{-# Language FlexibleInstances #-}
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

Furthermore items are labeled. The label stands for everything we know
about the item. The weight of a parcel may technically not be printed
on a label, but we treat it as part of the label nontheless. We make
no assumption about the label format, i.e. whether an item is
characterized by Format, Class and Destination or by anything else.

So an item with a certain type of Label |lty| is either
\begin{description}
\item[a nested item], i.e. one where we know what's inside. Such an
item is characterized by its own label and the items contained in it.
\item[a nonempty item], i.e. one where we don't know what's inside and
which is characterized by its own label only.
\end{description}

A nested Item is obviously a Tree, whereas a nonempty Item is a leaf
in that Tree. A leaf stands for an Item whose content is unknown, not
for an empty container. We known precisely what's inside an empty
container, namely nothing.

These consideration lead to the following definition:
\begin{code}

data Tree a = Node a | Tree a [Tree a]
              deriving (Eq, Ord, Show)

type Item lty = Tree lty

\end{code}

Note that the label type |lty| must be general enough to describe all
labels in our universe. We cannot e.g. define seperate label types for
trays and letters. In practice, this is not a limitation, because
different label types can always be united under one union-type.

\subsection{Products}

A Product tells us, whether or not it \emph{accepts} a given item.
Hence, at its heart, a Product is a \emph{predicate}, i.e. a function
|Item lty -> Bool|.

There a many datastructures which as suitable for implementing
|Product| behavior. We'll come to that later and for now we just
specifiy it as a typeclass (think:interface).

\needspace{12em} This leads to the following: There is a function
|accepts| which takes a Product and an Item and answers either |True|
or |False|.

Products and Items must agree on the label type |lty|. It must be
possible to compare labels for equality (|Eq|) and to order them
(|Ord|).


\begin{code}
class ProductI prod where
        accepts :: (Predicate pre, Eq lty) => 
                   prod (pre lty) -> Item lty -> Bool

\end{code}

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

\subsection{Product Implementation}

When you ask youself the question: "what does a sorting process
accept?", then the answer is: "everything that that can be sorted to
one of its outlets, excluding |NOT_IN_PLAN|. There is an implicit
\emph{or} condition in this definition.

Howevever, when you ask yourself: "what does an unpack process
accept", then the answer is: "everything that carries a specified
label, and whose content satisfies certain conditions".

The latter emphasizes a tree-like structure, wheras the former
emphasizes a collection of possibilities. But a Tree \emph{of what}?

The thing we're nesting or aggregating is a \emph{prdicate}. A
predicate is a function with a boolean return value. For our purposes,
we must also ask, that the predicate can be converted to a String (in
geneal a function cannot be converted to a String), otherwise we won't
be able to see the results of our Product-constructions.

\begin{code}
class Predicate p where
  prSat     :: (Eq a) => p a -> a -> Bool
  prAnd     :: (Eq a) => p a -> p a -> Maybe (p a)
  prOr     :: (Eq a) => p a -> p a -> p a
  prShow    :: (Show a) => p a -> String
\end{code}

A Product is then either a Tree of predicates or a list of such
Trees. Since constructing a List of something hardly warrants a
separate type, we simply say, that a Product is a Tree of predicates.


\begin{code}
type Product p = Tree p
\end{code}


Using the above definition, we can indeed construct a Product, which
again behaves like a predicate.


\begin{code}
\end{code}

Finally we need a suitable implementation for predicate. We need a way
to check if a labels matches certain conditions. The most simple
implementation is just a list of possible labels.

\begin{code}
data Labels lty = Labels [lty]
                   deriving (Show)

instance Predicate Labels where
  prSat  (Labels lbls) lbl = lbl `L.elem` lbls
  prAnd  (Labels lbls1) (Labels lbls2) =
    case L.intersect lbls1 lbls2 of
      [] -> Nothing
      ys -> Just (Labels ys)
  prOr (Labels lbls1) (Labels lbls2) =
          Labels (L.union lbls1 lbls2)
  prShow = show
 
\end{code}

Let's define a List with toplevel Labels "foo" and "bar", where
"foo" may contain "foo1" and "foo2"-labeled items and "bar" may
contain nothing at all. 

\begin{code}
-- shorthands
tree lbl xs = Tree (Labels [lbl]) xs
node lbl = Node (Labels [lbl])

ex_foo =  tree "foo" 
          [
            node "foo1",
            node "foo2"
           ]

ex_bar =  node "bar" 

ex_plist1 = [ex_foo, ex_bar]

-- The whole Product is a |ListRep|.
ex_prod1 = ListRep ex_plist1

\end{code}

\subsubsection{The Product instance}

We should now be able to define a |Product| instance of
|ProductRep|. Otherwise |ProductRep| would not be a suitable
implementation of |ProductI|. 


\begin{code}
-- Is item accepted by any of the predicates in the list ?
lAccepts :: (Predicate pre, Eq lty) => [Product (pre lty)] -> Item lty -> Bool
lAccepts pros item = any (flip tAccepts $ item) pros


-- Is item accepted by the single Product-Tree
tAccepts :: (Predicate pre, Eq lty) => Product (pre lty) -> Item lty -> Bool
tAccepts (Node preLty) (Node lbl)         = prSat preLty lbl
tAccepts (Node preLty) (Tree lbl _ )      = prSat preLty lbl
tAccepts (Tree preLty _) (Node lbl)       = False -- don't know what's inside
-- all items inside must be accepted
tAccepts (Tree preLty ps) (Tree lbl lbls) = prSat preLty lbl && all (lAccepts ps) lbls


\end{code}

Now all the |Product| instance has to do, is dispatch to either
|pAccepts| or |tAccepts|.

\begin{code}
data ProductRep pre lty = ListRep [Product (pre lty)] | 
                          NestRep (Product (pre lty))

instance ProductI (ProductRep Labels) where
        accepts (NestRep tProd) item  = tAccepts tProd item 
--         accepts (ListRep pros) item = lAccepts pros item
        

{-
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

% ------------------------------------------------------------                                                            
\section{Processes}
% ------------------------------------------------------------                                                            

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
input to the transformation.

Products which refer to a single container-Label are printed in red,
the others are printed in blue.

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
\includegraphics[width=5cm]{ProductsUnpack.eps}
\caption{Unpack}
\end{figure}

To compute the Product |yi|, we must know the container label |xc|. It
cannot be computed from anything, but needs to be specified by the
planner. However once set, this information travels "left" through
other processes, such that a potential upsream |Pack| Process will
know what label to use.

\subsection{Merge and Combine}

There are two different merging processes:
\begin{description}
\item[merge] unites mail, which can be in containers with different
  labels. An example is the merging of ordinary and priority mail for
  a Postman. The Postman accepts all mail for his beat and the merge
  process must specify that it accepts only |p1=priority| mail from
  one input and only |p2=ordinary| mail from the other. In an
  unconditional merge, i.e. one without the predicates |p1|, |p2| ...
  the input product is equal to the output product.
  \begin{figure}[htb!]
    \centering
    \includegraphics[width=5cm]{ProductsMerge.eps}
    \caption{Merge}
  \end{figure}

\item[combine] unites mail where each input accepts only items with a
  single label. An example is the merging of different route-trays
  into delivery-office-rollcontainers. The number of inputs is then
  not under the control of the planner, because the Product describing
  the rollcontainer already ``knows'' what routes can be accepted.
  \begin{figure}[htb!]
    \centering
    \includegraphics[width=5cm]{ProductsCombine.eps}
    \caption{Combine}
  \end{figure}
\end{description}


\subsection{Split}
There are two different splitting processes:
\begin{description}
\item[split] splits mail, which can be in containers with different
  labels. An example is the splitting of trays in a tray-sorter, or
  any other classic sorting process. A split process may specify
  additional predicates on its output, e.g. to restrict mail to
  certain formats or mail classes (attribute sorting). Without such
  predicates the process is completely determined by its output
  products.
\end{description}
\begin{figure}[htb!]
\centering
\includegraphics[width=5cm]{ProductsSplit.eps}
\caption{Split}
\end{figure}

\item[ind] is the dual of |combine|. A number of containers gets
  singluated into groups where each group carries a single label
  only. An example is the splittig of the contents of
  delivery-office-rollcontainers into route-trays (this is the process
  which typically follows the unpacking of rollcontainers).

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
container label |yc|, because the |Split| process accepts a |Plist|,
i.e. a union of things, with multiple possible container labels.

\section{Product Representations}

\subsection{General considerations}

The |Product| typeclass defines the behavior of Products in a somewhat
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

\subsection{Set Operations}

Let's now define some Set-operations.

\subsubsection{Union}

The union of two |Plists| can be computed by buiding the union of the
underlying lists. Other than that, there are some obvious corner cases
concerning |Pany| and |Pnone|\footnote{to DTZ: Plist is not a Monoid, as it required Ord.}.

\begin{code}
lUnion :: Ord a => Plist a -> Plist a -> Plist a
lUnion PlAny _  = PlAny
lUnion _ PlAny  = PlAny
lUnion (Plist as) (Plist bs) = Plist (L.union as bs)
\end{code}


\subsubsection{Intersection}

Two |pNests| can be intersected, which may or may not produce a
result. Disjoint |pNests| will produce Nothing. The operation calls
|lIntersection| to intersect the possible contained items.

\begin{code}
nIntersection :: (Ord a) => Pnest a -> Pnest a -> Maybe (Pnest a)
nIntersection (Pnest a as) (Pnest b bs)
  | a == b    = Just $ Pnest a (lIntersection as bs) 
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
One can filter a |Plist| with a |pNest| such that only those list
items prevail, which are part of the |pNest|.

\begin{code}
lFilter :: (Ord a) => Pnest a -> Plist a -> Plist a
lFilter pn PlAny = Plist [pn]
lFilter pn (Plist pns) = Plist $ foldr f [] pns
  where
    f pn' ys = case nIntersection pn pn' of
      (Just y') -> y':ys
      otherwise -> ys
\end{code}

\needspace{12em}
\begin{run} 
We can filter our |ex_plist1| such that only "foo1" inside a "foo" are
allowed. No more "bar" toplevels and no more "foo2" inside a "foo" are
accepted.

|*Main> lFilter (Pnest "foo" (Plist [Pnest "foo1" PlAny])) ex_plist1|
  \eval{lFilter (Pnest "foo" (Plist [Pnest "foo1" PlAny])) ex_plist1}
\end{run}


\needspace{12em}
Finally the intersection of |Plist|. Basically we build the union of
filtering the second |Plist| by every |pNest| in the first |Plist|.

\begin{code}
lIntersection :: Ord a => Plist a -> Plist a -> Plist a
lIntersection PlAny x  = x
lIntersection x PlAny  = x
lIntersection (Plist pcks1) pls = foldr lUnion (Plist []) $ do
  pck1 <- pcks1
  return $ lFilter pck1 pls
\end{code}


\subsection{Processes transforming Products}

Because we destinguished between |Plist| and |Pnest| we can now be
specific about what kind of Product each Process transforms and what
the type of the transformed Product is. It will not be possible to
accidently place a |Split| afer a |Pack|, because (as we shall see)
|Split| computes a |Plist|, which is not transformed by a |Pack|.


\subsubsection{Split}

|Split| takes a number of |Plists|, which stand for the Products which
are accepted e.g. by the Stackers, and computes the Product which is
accepted by the feeder of the machine. The computed Product is again a
|Plist|.

\begin{code}
split :: (Ord a)=> [Plist a] -> Plist a
split pxs = foldr lUnion (Plist []) pxs
\end{code}

\subsubsection{Merge}

|Merge| does the inverse operation, but as stated earlier, it needs
additional information to decide, what to accept at each input. This
additional information comes in the form of a list of |Plist|, one
for each input. The result of the computation is a list of |Plist|,
again one for each input.

\begin{code}
merge :: (Eq a, Ord a) => [Plist a] -> Plist a -> [Plist a]
merge plss pls = map f plss
  where
    f p = lIntersection p pls

-- xxx
mergeAll :: (Eq a, Ord a) => Plist a ->[Pnest a]
mergeAll (Plist pns) = pns

\end{code}

\subsubsection{Pack} |Pack| takes a |Pnest| and computes the
container label and the |Plist| for the items it accepts. Essentially
it removes one level of nesting.

\begin{code}
pack :: Pnest lty -> (lty, Plist lty)
pack (Pnest lbl pls) = (lbl, pls)
\end{code}

\subsubsection{Unpack}
|Unpack| takes a container label and a |Plist| and produces a |Pnest|.


\begin{code}
unpack :: lty -> Plist lty -> Pnest lty
unpack lbl pls = Pnest lbl pls
\end{code}

\section{Larger examples}

I got stuck here. Consider a rollcontainer containing trays for 10
routes. You unpack the rollcontainer and then split the trays into two
parts, one containing the odd routes and the other containing the even
routes. You cannot sensibly unpack any of the outputs, because Unpack
assumes all containers carry the same label. Only when you split the
trays into 10 routes you get ``pure'' trays which can be
unpacked. Could it be that there is a special Split operation, which
splits by label?

Something is not right with our elementary functions.

\begin{figure}[htb!]
\centering
\includegraphics[width=12cm]{ProductsExRec.eps}
\caption{Pack}
\end{figure}

\begin{code}
leaf :: lty -> Plist lty
leaf lbl = Plist[Pnest lbl PlAny]

splitAll :: Ord a => [Pnest a] -> Plist a
splitAll ns = split $ map (\np -> Plist [np]) ns
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
                               ] :: Plist Ex_lbl
           -- Each route has its own |Pnest| tray product
           rTray route   = unpack (clbl "Tray" "Route" route) (rRoute route)      
                         :: Pnest Ex_lbl

           -- Each delivery office is responsible for 10 routes ...
           rDof dof    = splitAll [rTray route | route <- range 10 dof] 
                       :: Plist Ex_lbl

           -- ... and has a dedicated rollcontainer
           rRc n     = unpack (clbl "RollContainer" "DO" n) (rDof n)  
                     :: Pnest Ex_lbl

           -- a Region services 5 Delivery Offices ...
           rRegion reg = splitAll [rRc i | i <- range 5 reg]     
                       :: Plist Ex_lbl

           -- ... and has a truck bringing the rollcontainers
           rTruck n  = unpack (clbl "Truck" "Region" n) (rRegion n)   :: Pnest Ex_lbl

           ------------------------------------------------------------
           -- Now what happens at the departure side of the truck
           ------------------------------------------------------------
           -- The truck to region n gets packed
           sTruck n = pack(rTruck n)               :: (Ex_lbl, Plist Ex_lbl)

           -- Each rollcontainer comes from a different source ...
           sRcs reg  = mergeAll (snd (sTruck reg))     :: [(Pnest Ex_lbl)]

           -- ... where they get packed
           sDof reg dof  = pack (sRcs reg !! dof)          :: (Ex_lbl, Plist Ex_lbl)

           -- 

           in (sRcs 0) -- (stray 0) !! 0
-}
\end{code}

%\begin{figure}[htb!]
%\centering
%\includegraphics[width=4cm]{glass-slipper.jpg}
%\end{figure}

\end{document}
