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
pp a = putStrLn $ ppShow a
\end{code}
%endif

\section{Items}
Mail items can be letters, parcels, trays, rollcontainers, truck and
many more. It appears like some of them are containers and others are
\emph{atomic}. However emphasizing the distinction between containers
and atomic items turned out to be suboptimal, primarily because this
distinction lies in the eye of the beholder and is not a true property
of a mail item.

For a parcel processing company a parcel appears like an atomic
item. However it is certainly a container and it contains other
items. It is just the parcel company not caring about what's inside,
which makes it appear atomic.

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

However, the above definition would not allow \emph{printing}
Products, i.e. converting Products to Strings (you cannot convert a
function to a String). Since our goal is to construct Sorting
Products, we better be able to print the result.

So we need a \emph{representation} of a Product, which behaves like a
predicate and also allows printing it. In other words, we can look at
a Sorting Product from two angles: first there is its predicate
behaviour and second there is its textual representation.

Textual representations of Sorting Products are ubiquitous in the
postal world. They appear e.g. in Sortplan files. ADM-SPM has it's own
internal representation, called |Expands_To|.

If we separate the predicate aspect from its representation, then any
representation must adhere to the constraint that it must allow being
treated like a predicate.

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

\subsection{Representation as MapProducts}

There are countless ways to represent a Sorting Product. We chose one
which is based on a |Map|. 

A |MapProduct| is a Map whose keys are the toplevel container labels
we accept, and whose values are the |MapProducts| we accept inside the
respective container. Note that we cannot simply use the labels of the
contained items as values, as this would allow only one level of
nesting.

Furthermore, we need a way to express, that we don't care about what's
inside a conainer. We give this MapProduct a special constructor,
called |MPany|.

This leads to the following definition

\begin{code}
data  MapProduct lty = MPacked  (M.Map lty (MapProduct lty)) | 
                       MPany 
                     deriving (Eq, Show)
\end{code}

\subsection{MapProducts are Products}

We must now be able to implement the function |sat|, or our MapProduct
does not pass as a Product. This is indeed possible. The reasoning is
the following:

\begin{itemize}
\item if the MapProduct is |MPany| then |sat| always returns |True|
  regardless of what Item was passed.
\item if the MapProduct is an |MPacked| and the Item is an |Ipacked|,
  then we must be able to lookup the toplevel label. As a result we
  get another another MapProduct |mp| and \emph{all} contained Items must fall
  into this |mp|.
\item if the MapProduct is again an |MPacked|, but this time the Item
  is an |INonempty|, then again we must be able to lookup the toplevel
  label, but this time the returned MapProduct \emph{must} be an
  |MPany|, because if we don't know what's inside the container we
  can only accept the Item if we don't care about what's inside.
\end{itemize}
\needspace{4em}
This leads to the following implementation of |sat|
\begin{code}
instance Product MapProduct
        where
            sat MPany _ = True
            sat (MPacked map) (Ipacked lbl items) 
                    = case M.lookup lbl map of
                          Nothing -> False
                          Just mp -> all (sat mp) items
            sat (MPacked map) (Inonempty lbl)
                    = case M.lookup lbl map of
                          Just MPany -> True
                          _          -> False

\end{code}

\subsection{Examples}

So far we have not yet defined any convenience functions for
constructing MapProducts, so our Products look a bit verbose. 

Here is an example of a MapProduct with the toplevel labels ``foo''
and ``bar'', where ``foo'' may contain items with the labels ``foo1''
or ``foo2'' and accordingly for ``bar''.

We construct the Product by inserting elements into a Map, starting
with an empty Map.

\begin{code}
ex_prod1 = let (>>) = flip ($)
           in
            M.empty
            >> M.insert "foo" (
              M.empty
              >> M.insert "foo1" MPany
              >> M.insert "foo2" MPany
              >> MPacked
                              )
            >> M.insert "bar" (
              M.empty
              >> M.insert "bar1" MPany
              >> M.insert "bar2" MPany
              >> MPacked
                              )
            >> MPacked
\end{code}

\medskip

\begin{run}
An empty ``foo'' Item matches

|*Main> sat ex_prod1 (Ipacked "foo" [])| \\
  \eval{sat ex_prod1 (Ipacked "foo" [])}

However, a nonempty ``foo'' does not match, because we only accept
``foo1'' or ``foo2'' inside a ``foo''.

|*Main> sat ex_prod1 (Inonempty "foo")| \\
  \eval{sat ex_prod1 (Inonempty "foo")}

A ``foo'' with a ``foo1'' inside matches

|*Main> sat ex_prod1 (Ipacked "foo" [Ipacked "foo1" []])| \\
  \eval{sat ex_prod1 (Ipacked "foo" [Ipacked "foo1" []])}

A ``foo'' with a ``bar1'' inside does not match

|*Main> sat ex_prod1 (Ipacked "foo" [Ipacked "bar1" []])| \\
  \eval{sat ex_prod1 (Ipacked "foo" [Ipacked "bar1" []])}

But a ``bar'' with a ``bar1'' inside matches

|*Main> sat ex_prod1 (Ipacked "bar" [Ipacked "bar1" []])| \\
  \eval{sat ex_prod1 (Ipacked "bar" [Ipacked "bar1" []])}

The ``bar1'' can be nonempty, because we don't care what's inside

|*Main> sat ex_prod1 (Ipacked "bar" [Inonempty "bar1"])| \\
  \eval{sat ex_prod1 (Ipacked "bar" [Inonempty "bar1"])}

We can even stick an "xxx" into the bar1 and it still matches

|*Main> sat ex_prod1 (Ipacked "bar" [Ipacked "bar1" [Inonempty "xxx"]])| \\
  \eval{sat ex_prod1 (Ipacked "bar" [Ipacked "bar1" [Inonempty "xxx"]])}
\end{run}

\section{Product Transformations}
We shall now look at the various processes in the postal world and
examine how they transform Sorting Products. We'll use diagrams to
illustrate the processes. 

The arrows in those diagrams point in the direction of the Item
flow. The transformation of Sorting Products however, runs in the
opposite direction, i.e. the input Products are computed from the
output products. To emphasize this, we label the input products with
``y'', as they are the result of the transformation abd the output
products with ``x'', as they are the input to the transformation.
 
\subsection{Intersection}

As a warmup, let's define the intersection between two
MapProducts. The resulting Product shall accept all items, which are
accepted by both of the Products.

To do this, we first find the keys which are present in both
maps. Each key is associated with another MapProduct and we intersect
these. There are two easy corner cases, where one of the Products is
an |MPany|.

\begin{code}
mpIntersect mp1 MPany = mp1
mpIntersect MPany mp2 = mp2
mpIntersect (MPacked map1) (MPacked map2)
                      = MPacked (M.intersectionWith mpIntersect map1 map2)
\end{code}

\subsubsection{Examples}

This Product only has \emph{"foo1" inside "foo"} in common with the
already defined |ex_prod1|.

 \begin{code}
ex_prod2 = let (>>) = flip ($)
           in
            M.empty
            >> M.insert "foo" (
              M.empty
              >> M.insert "foo1" MPany
              >> M.insert "foo3" MPany 
              >> MPacked
                              )
            >> MPacked
\end{code}

\needspace{4em}
Intersecting |ex_prod1| with |ex_prod2| indeed returns the expected result.
\medskip
\begin{run}
|*Main> mpIntersect ex_prod1 ex_prod2| \\
  \eval{mpIntersect ex_prod1 ex_prod2}
\end{run}


\subsection{Restrict}  
Think of \emph{restrict} as specifying things like \emph{but only
  priority mail}. There is not really a process capable of doing that,
but it is handy nontheless.

 \begin{figure}[htb!]
\centering
\includegraphics[width=6cm]{ProductsRestrict.eps}
\caption{Restrict}
\end{figure}

\emph{Restrict} takes a predicate |lty->Bool| and removes all keys
from the map, which do not satisfy the predicate.  \emph{Restrict}
does not affect the condition for contained items and it cannot handle
|MPany|.

On the other hand, it is more powerful the |mpIntersect| because a
predicate is more powerful than a Map, where you have to specify each
key you want included.


\begin{code}
mpRestrict :: (a->Bool) -> MapProduct a -> MapProduct a
mpRestrict pred  (MPacked map) = MPacked $ M.filterWithKey f map
                                  where
                                    f k v = pred k

\end{code}

\subsubsection{Examples}
\begin{run}
We restrict |ex_prod1| such that only the ``bar'' labeled Items are
accepted. The next level, i.e. the contained items are not affected.

|*Main> mpRestrict (== "bar") ex_prod1| \\
  \eval{mpRestrict (== "bar") ex_prod1}
\end{run}




%\begin{figure}[htb!]
%\centering
%\includegraphics[width=4cm]{glass-slipper.jpg}
%\end{figure}

\end{document}
