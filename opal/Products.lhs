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
\newmdenv[frametitle=Output,backgroundcolor=gray!05,roundcorner=2pt]{run}

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
import qualified Data.Map as M
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

\section{Items}
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
printed on the label, but we treat it as part of the label
nontheless. We make no assumption whether an item is characterized by
Format, Class and Destination or by anything else. 

So an item with a certain type of Label |lty| is either
\begin{description}
\item[a packed item], i.e. one where we know what's inside. Such an
item is characterized by its own label and the items contained in it.
\item[a nonempty item], i.e. one where we don't know what's inside and
which is characterized by its own label only.
\end{description}

The \emph{nonempty} items are in a way the \emph{atomic} items we
talked about earlier. There is a clear distinction between such
nonempty/atomic items and an empty container. For an empty container,
we known precisely what's inside, namely nothing.

These consideration lead to the following definition:
\begin{code}
data Item lty = Ipacked lty [Item lty] | 
                Inonempty lty
              deriving (Eq, Ord, Show)
\end{code}

Note that the label type |lty| must be general enough to describe all
labels in our universe. We cannot e.g. define seperate label types for
trays and letters.

\section{Processes}

There are six elementary Processes, namely \emph{Merge, Split, Pack,
Unpack, Buffer} and \emph{Transport}. Only the first four transform
Products, while the last two leave them unchanged, at least when you
assume that the time and place of an Item does not matter.

You may be tempted to believe that these Procersses can be combined in
an arbitrary way. But this is not the case. Also it is in general not
possible to compute input Products from output Products alone.

\subsection{Pack}

 \begin{figure}[htb!]
\centering
\includegraphics[width=5cm]{ProductsPack.eps}
\caption{Pack}
\end{figure}

Pack takes Items and places them inside a Container with a specified
label |yc|. You can compute |yc| and |yi| only whem |xi| accepts containers
whith a single label. In constrast, the Product at the input of a
\emph{Split} Process (see below), may accept Items with various labels.

\subsection{Unpack}

 \begin{figure}[htb!]
\centering
\includegraphics[width=5cm]{ProductsUnpack.eps}
\caption{Unpack}
\end{figure}

To compute the Product |yi|, we must know the container label |xc|. It
cannot be computed from anything, but needs to be specieifed by the
planner. However once set, this information travels "left" through
other processes, such that a potential |Pack| Process will know what
label to use. Don't confuse this label with a piece of paper. It
describes "everything we know about the container".

xxx It is stange that we can express an empty container as |Packed lbl
[]| but here we jast take the container label to describe an empty
container.

\subsection{Merge}
 \begin{figure}[htb!]
\centering
\includegraphics[width=5cm]{ProductsMerge.eps}
\caption{Merge}
\end{figure}

When given an output Product |xi| it is not possible to compute the
input products |yi1| and |yi2| without further information. First you
wouldn't know how many inputs there are and second, just passing |xi|
to all inputs would be a perfectly valid solution.

\begin{figure}[htb!]
\centering
\includegraphics[width=5cm]{ProductsMerge2.eps}
\caption{Merge as Intersections}
\end{figure}

The least thing you need to do is to sepecify what you want from each
input. The resulting |yi| Products can then be computed, such that
each item that is accepted, is accepted both by the respective |p| and
|xi|, i.e.

\begin{eqnarray}
yi1 = p1 \cap xi \\
yi2 = p2 \cap xi
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

Split appears easy, because its input Product is naturally just the
union of its output products.

\begin{eqnarray}
py = px1 \cup px2
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

\section{Products}

\subsection{Definition}

A Product tells us for each item whether or not it \emph{falls into
the product}. In that respect a Product is a \emph{predicate}. This
would suggest, that a Product is just a function |Item lty ->
Bool|. 

\needspace{12em}
This leads to the following typeclass (think: interface): A Product is
something which implements a function |sat|. That function takes a
Product and an Item and answers either |True| or |False|. Products and
Items must agree on the label type |lty| and we ask labels to be
sortable (|Ord|).

\begin{code}
class Product prod where
        sat :: (Ord lty) => prod lty -> (Item lty) -> Bool

\end{code}

\subsection{Product representations}

However, the above definition does not buy us much. Particularly it
would not allow \emph{printing} Products, i.e. converting Products to
Strings (you cannot convert a function to a String). Since our goal is
to construct Sorting Products, we better be able to print the result.

So we need a \emph{representation} of a Product, which behaves like a
predicate and also allows printing it. In other words, we can look at
a Sorting Product from two angles: first there is its predicate
behaviour and second there is its textual representation.

Textual representations of Sorting Products are ubiquitous in the
postal world. They appear e.g. in Sortplan files. ADM-SPM has it's own
internal representation, called |Expands_To|.

We will mostly be concerned about concrete implementations of
Products. 

\subsection{Products for Items with negative value}

When you buy a commercial Product, e.g. an Amazon Firestick, you
expect to receive the Firestick itself, a remote control a power
supply, two batteries and an HDMI extender cable. If any of these
items are \emph{missing}, you have reason to complain. This is because
all these items have a value greater than zero. You would however, not
complain when there were some extra items in the package, like some
flyers or candy.

When dealing with mail, things are different. Mail items have a value
less than zero. A process in the chain will not complain when it
receives nothing at all. It only complains, when it receives something
it doesn't want, just like you, as a mail recipient, will
complain when you receive mail which is not for you.

So Sorting Products do no specify what must be included, but what must
not be included.

\section{A Product Representation}

The above considerations about Processes suggest, that there are two
kinds of Products. One describes a container with its possible
conents, the other one descibes alternatives. 

When you consider the address on an envelope as an Item-Label, then
each Stacker in a Sorting machine accepts many alternative labels. The
Process as a whole also accpepts (even more) alternatives.

We therefore distinguish between \emph{List} Products to desribe
alternatives and \emph{Pack} Products to describe containers. These
two mutually reference each other. This leads to the following:

\begin{code}
data Plist lty    = Plist [Packed lty] | PlAny | PlNone
                  deriving (Eq, Ord, Show)
data Packed lty   = Packed lty (Plist lty)
                  deriving (Eq, Ord, Show)
\end{code}

A |Plist| is basically just a list of |Packed|, with the two
additional constructors |PlAny| and |PlNone|, which match anything or
nothing respectively.

A |Packed| is defined by a container label of type |lty| and the
possible content of the container, which is defined as a |Plist|.

\subsection{Set Operations}

This definition allows defining some set operations.

\subsubsection{Union}

The union of two |Plists| can be computed by buiding the union of the
underlying lists. Other than that, there are some obvious corner cases
concerning |Pany| and |Pnone|

\begin{code}
plsUnion :: Ord a => Plist a -> Plist a -> Plist a
plsUnion PlAny _  = PlAny
plsUnion _ PlAny  = PlAny
plsUnion PlNone x = x
plsUnion x PlNone = x
plsUnion (Plist as) (Plist bs) = Plist (L.union as bs)
\end{code}


\subsubsection{Intersection}

Two |Ppacked| can be intersected, which may or may not produce a
result. Disjoint |Ppacked| will produce Nothing. The resulting
|Ppacked| has the property, that any item that matches it, matches
both of the input |Packed|. The operation calls |plsIntersect| to
intersect the possible contained items.

\begin{code}
pckIntersect :: (Ord a) => Packed a -> Packed a -> Maybe (Packed a)
pckIntersect (Packed a as) (Packed b bs)
  | a == b    = Just $ Packed a (plsIntersect as bs)
  | otherwise = Nothing
\end{code}

One can filter a |Plist| with a |Ppacked| such that only those list
items prevail, which are part of the |Ppacked|.

\begin{code}
plsFilter :: (Ord a) => Packed a -> Plist a -> Plist a
plsFilter pck PlAny = Plist [pck]
plsFilter _ PlNone  = PlNone
plsFilter pck (Plist pcks) = Plist $ foldr f [] pcks
  where
    f pck' ys = case pckIntersect pck pck' of
      (Just y') -> y':ys
      otherwise -> ys

\end{code}

Finally the intersection of |Plist|. Basically we build the union of
filtering the second |Plist| by every |Ppacked| in the first |Plist|.

\begin{code}
plsIntersect :: Ord a => Plist a -> Plist a -> Plist a
plsIntersect PlAny x  = x
plsIntersect PlNone x = PlNone
plsIntersect x PlAny  = x
plsIntersect x PlNone = PlNone
plsIntersect (Plist pcks1) pls = foldr plsUnion (Plist []) $ do
  pck1 <- pcks1
  return $ plsFilter pck1 pls
\end{code}


\subsection{Processes transform Products}

Because we destinguished between |Plist| and |Packed| we can now be
specific about what kind of Product each Process transform and what
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
split pxs = foldr plsUnion (Plist []) pxs
\end{code}

\subsubsection{Merge}

|Merge| does the inverse operation, but as stated earlier, it needs
additional information to decide, what to accept at each input. This
additional information comes in the form of a list of |Packed|, one
for each input. The result of the computation is a list of |Plist|,
again one for each input.

xxx wrong: additional info must be |Plist|
\begin{code}
merge :: (Eq a, Ord a) => [Packed a] -> Plist a -> [Plist a]
merge pcks pls = map f pcks
  where
    f p = plsFilter p pls

merge' :: (Eq a, Ord a) => [Plist a] -> Plist a -> [Plist a]
merge' plss pls = map f plss
  where
    f p = plsIntersect p pls
\end{code}

\subsubsection{Pack} |Pack| takes a |Packed| and computes the
container label and the |Plist| for the items it accepts. Essentially
it removes one level of nesting.

\begin{code}
pack :: Packed lty -> (lty, Plist lty)
pack (Packed lbl pls) = (lbl, pls)
\end{code}

\subsubsection{Unpack}
|Unpack| takes a container label and a |Plist| and produces a |Packed|.


\begin{code}
unpack :: lty -> Plist lty -> Packed lty
unpack lbl pls = Packed lbl pls
\end{code}

\begin{code}
\end{code}

\section{tests}

%\begin{figure}[htb!]
%\centering
%\includegraphics[width=4cm]{glass-slipper.jpg}
%\end{figure}

\end{document}
