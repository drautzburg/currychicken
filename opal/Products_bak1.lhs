\documentclass[a4paper]{article}
%include lhs2TeX.fmt
%include greek.fmt
%options ghci -fglasgow-exts

\usepackage{float}
\usepackage{titlesec}
\newcommand{\sectionbreak}{\clearpage}
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
\RaggedRight
\linespread{1.25}
\newmdenv[
  frametitle=Note,
  backgroundcolor=gray!05,
  topline=false,
  bottomline=false,
  skipabove={1em},
  skipbelow={1em},
  needspace=12em,
  innerrightmargin={2em}
]{note}

\newmdenv[
  frametitle=Output:,
  backgroundcolor=white,
  topline=false,
  bottomline=false,
  skipabove={1em},
  skipbelow={1em},
  needspace=6em
]{run}


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


%if false
\begin{code}
{-# Language FlexibleInstances #-}
import qualified Data.List as L
import qualified Data.Set as S
import qualified Data.Monoid as M
import Test.QuickCheck
import Data.Maybe
import Data.Function
import qualified Data.Ord as O
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

% ------------------------------------------------------------
\section{Items and Products}
% ------------------------------------------------------------

\subsection{Items}

Mail items can be letters, parcels, trays, roll-containers, trucks and
many more. It appears like some of them are containers and others are
\emph{atomic}. However, emphasizing the distinction between containers
and atomic items is misleading, primarily because this distinction
lies in the eye of the beholder and is not a true property of a mail
item.

For a parcel processing company a parcel appears like an atomic
item. However, it is certainly a container and it does contain other
items. It only appears atomic, because the parcel company does not
care about what's inside.

Therefore we treat all items as containers which could potentially
contain other items. We need an additional means of expression for
items where we don't care (or know) what's inside.

Furthermore items are labeled.

\begin{note}
The label stands for everything we need to now about the item,
including things like the container type.
\end{note}

The weight of a parcel may technically not be printed
on a label, but we treat it as part of the label nontheless. We make
no assumption about the label format, i.e. whether an item is
characterized by Format, Class and Destination or by anything else.

So an item with a certain type of Label |lty| is either
\begin{description}
\item[an item with known content.] Such an item is characterized by
  its own label and the items contained in it.
\item[an item with unknown content] (``nonempty item''), which is
  characterized by its own label only.
\end{description}

A nested Item is obviously some kind of Tree, and a nonempty Item is a
leaf in that Tree. A leaf stands for an Item whose content is unknown,
not for an empty container. We known precisely what's inside an empty
container, namely nothing.

These considerations lead to the following definition:
\begin{code}

data Tree a = Leaf a | 
              Node a [Tree a]
              deriving (Eq, Show) 

type Item lty = Tree lty

-- Trees can be ordered, Leafs are greater than anything
instance (Ord t) => Ord (Tree t)
  where
    compare (Node x xs) (Node y ys)  = O.compare xs ys
    compare (Leaf x) (Leaf y)        = O.compare x y
    compare (Leaf x) _               = GT
    compare _ (Leaf x)               = LT
\end{code}

\begin{note}
Items with unknown content are Leaves in a Tree. Items with known
content are Nodes in a Tree.
\end{note}



Note that the label-type |lty| must be general enough to describe all
labels in our universe. We cannot e.g. define seperate label types for
trays and letters. In practice, this is not a limitation, because
different label types can always be united under one union-type.

\subsection{Products}
\subsubsection{Introduction}
A Product tells us, whether or not it \emph{accepts} a given item.
Hence, at its heart, a Product is a function |Item lty -> Bool|.

Now, something must be assumed about the ingredients a Product is
built from. Our reasoning is as follows: somehow you must be able to
test whether a labels is equal to something, otherwise it will be
impossible to tell whether a nested thing like an Item matches some
criteria. Since we want to leave the actual label-type unspecified, we
settle for a predicate over anything which can be compared for
equality.

\begin{code}
class IPredicate p where
  prSat          :: (Eq a) => p a -> a -> Bool

  prShow         :: (Show a) => p a -> String
  prAnd          :: (Eq a) => p a -> p a -> p a
  prOr           :: (Eq a) => p a -> p a -> p a
  prAny          :: p a
  prNone         :: p a
\end{code}

You see, we asked for a number of other things besides the main
function |prSat|. This is best understood by examining what |SOP|, the
Sortcode-range-building component in ADM-SPM does. \emph{All} that SOP
does is to manipulate such simple predicates. It has no concept of
nested items. But it has a concept of printing the results of such
manipulations (|prShow|), otherwise we could not generate Sortplans,
and it can do some boolean operations.

SOP does this using a particular encoding for predicates, called
|EXPANDS_TO|. Though we do not want to use such an encoding here, we
want to be able to do so later. Hence we cannot be too specific about
the encoding of a predicate and therefore we just specify the
interface.

Items are really nested things, but a simple predicate on labels does
not take any nesting into account. The next thing we need to do, is to
lift the capability to check Labels to the capability to check Items,
however nested they may be.

This is what we ask from a Product: Given a predicate |pre| and a
label type |lty|, construct something which implements the function
|accepts|\footnote{You might be tempted to think, that |IProduct| is
  also an |IPredicate|, because |accepts| maps an |Item| to a Bool. So
  shouldn't a Product be a "Predicate for Items"? This is not the
  case, because |Predicate| works on everything which can be compared
  for equality ("|(Eq a)=>|"), but |Product| is taylored specificly to
  the kind of nesting we expect from Items. Hence we cannot say that
  |Product| is a Predicate for \emph{everything}, which can be
  compared for equality.}.

\begin{code}
class IProduct prod where
        accepts :: (IPredicate pred, Eq lty) => 
                   prod (pred lty) -> Item lty -> Bool

\end{code}

\begin{note}
|pred lty| is a predicate over labels of type |lty|. You may see this
as the set of labels, which are acceptable. 
\end{note}


\subsubsection{Product Implementation}

When you ask youself the question: "what does a sorting process
accept?", then the answer is: "everything that that can be sorted to
one of its outlets (|NOT_IN_PLAN| excluded). There is an implicit
\emph{or} condition in this definition.

Howevever, when you ask yourself: "what does an unpack process
accept", then the answer is: "everything that carries certain labels,
and whose content satisfies certain conditions".

The latter emphasizes a tree-like structure, wheras the former
emphasizes a collection of possibilities. So we need to combine
predicates in two ways: as a |Tree| or as a list of |Trees|

\begin{note}
A Product describes either
\begin{itemize}
\item a container, i.e. what container-labels are accepted and what
  can be in the container. This is a |Tree| of predicates
\item a choice between containers. This is a |List| of containers,
  i.e. a List of Trees.
\end{itemize}
\end{note}
So:

\begin{code}
data Product pred = ProdTree (Tree pred) |
                    ProdList [Tree pred]
                    deriving (Show)
\end{code}

We can always extract a List from a Product, a |ProdTree| then turns
into a List with one element.

\begin{code}
prodToList (ProdTree x)  = [x]
prodToList (ProdList xs) = xs
\end{code}

And we can always extract a "head" from a Tree no matter if we get a
|Node| or a |Leaf|. The head is the predicate on labels.
\begin{code}
treeHead (Node x xs) = x
treeHead (Leaf x)    = x
\end{code}

Finally we need a suitable encoding for a predicate on labels. The
most simple implementation is just a list of possible labels.

\begin{code}
data Labels lty = Labels [lty] | AnyLabel
                   deriving (Eq, Ord, Show)

\end{code}

This is indeed a predicate, because we can implement all the functions
required by |IPredicate|
 
\begin{code}
instance IPredicate Labels where
  prSat  AnyLabel _ = True
  prSat  (Labels lbls) lbl = lbl `L.elem` lbls

  prAnd AnyLabel y = y
  prAnd  (Labels lbls1) (Labels lbls2) = Labels $ L.intersect lbls1 lbls2
      
  prOr AnyLabel y = AnyLabel
  prOr (Labels lbls1) (Labels lbls2) =
          Labels (L.union lbls1 lbls2)

  prShow = show

  prAny  = AnyLabel
  prNone = Labels []

\end{code}

Ans it it something, which can be appended to (a Monoid)
\begin{code}
instance (Eq t) => M.Monoid (Labels t) where
  mappend AnyLabel _ = AnyLabel
  mappend _ AnyLabel = AnyLabel
  mappend (Labels x) (Labels y) = Labels (L.union x y)
  
  mempty = Labels []
\end{code}
In order to convince ourselves that |Product| satisfies the
constraints defined in |IProduct|, we'll now implement |accepts|. We
start by implementing accept functions for Trees and Lists of Trees
separately.

\begin{code}
-- Is item accepted by any of the Products in a list?
lAccepts :: (IPredicate pred, Eq lty) => 
            [Tree (pred lty)] -> Item lty -> Bool
lAccepts prods item = any (flip tAccepts $ item) prods
\end{code}
\needspace{12em}
\begin{code}
-- Is item accepted by a single Product-Leaf?
tAccepts :: (IPredicate pred, Eq lty) => 
            Tree (pred lty) -> Item lty -> Bool
tAccepts (Node pred ls) (Node ilbl items) = prSat pred ilbl &&
                                          -- all items inside must be accepted
                                          all (lAccepts ls) items
tAccepts (Leaf pred) item                 = prSat pred (treeHead item)
tAccepts (Node pred _) (Leaf _)           = False -- don't know what's inside 

\end{code}

To implement |accepts| we just need to dispatch to either |pAccepts|
or |tAccepts|.

\begin{code}
instance IProduct Product where
         accepts (ProdList l) = lAccepts l
         accepts (ProdTree t) = tAccepts t

\end{code}


\subsubsection{Examples}

Let's define a |ProdList| with toplevel Labels "foo" and "bar", where
"foo" may contain "foo1" and "foo2"-labeled items and "bar" may
contain nothing at all. 

\begin{code}
ex_plist1 = 
           [ Node (Labels [ "foo" ]) [ Leaf (Labels [ "foo1" , "foo2" ]) ],
             Node (Labels [ "bar" ]) []
           ]
ex_prod1 = ProdList ex_plist1

\end{code}


\medskip
\begin{run}
We don't accept a nonempty "foo", because we only allow "foo1" or
"foo2" inside, but we don't know what's inside the nonempty ``foo''.

|*Main> accepts ex_prod1 (Leaf "foo")|\\
  \eval{accepts ex_prod1 (Leaf "foo")}

\needspace{10em}
However, a "foo" with a "foo1" inside is accepted.

|*Main> accepts ex_prod1 (Node "foo" [Leaf "foo1"])|\\
  \eval{accepts ex_prod1 (Node "foo" [Leaf "foo1"])}

\needspace{10em}
But not when it also contains a "lint"

|*Main> accepts ex_prod1 (Node "foo" [Leaf "foo1", Leaf "lint"])|\\
  \eval{accepts ex_prod1 (Node "foo" [Leaf "foo1", Leaf "lint"])}

\needspace{10em}
An empty "foo" is accepted too

|*Main> accepts ex_prod1 (Node "foo" [])|\\
  \eval{accepts ex_prod1 (Node "foo" [])}

\needspace{10em}
And so is an empty "bar"

|*Main> accepts ex_prod1 (Node "bar" [])|\\
  \eval{accepts ex_prod1 (Node "bar" [])}

\needspace{10em}
But a nonempty "bar is not, because "bar" must be empty

|*Main> accepts ex_prod1 (Leaf "bar")|\\
  \eval{accepts ex_prod1 (Leaf "bar")}
\end{run}

\needspace{12em}


\subsection{Items with negative value}

When you buy a commercial Product, e.g. an Amazon Firestick, you
expect to receive the Firestick itself, a remote control a power
supply, two batteries and an HDMI extender cable. If any of these
items are \emph{missing}, you have reason to complain and you may
refuse to accept the item. You would however, not complain when there
were some extra items in the package, like some candy. This is because
these these extra items have a value greater than zero.

When dealing with mail, things are different. Mail items have a value
less than zero. A process in the chain will not complain when it
receives nothing at all. It only complains, when it receives something
it doesn't want, just like you, as a mail recipient, will
complain when you receive mail which is not for you.

So logistic (``Sorting'') Products do not specify what must be
included, but what \emph{may} be included. Or alterternatively, they
specify what must \emph{not} be included.




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

You may be tempted to believe that these Processes can be combined in
arbitrary ways. But this is not the case. Also, there are cases where
it is not possible to compute input Products from output Products
alone. We shall try to define our types, so these things become
obvious.

We'll use diagrams to illustrate the processes.  The arrows in those
diagrams point in the direction of the Item flow. The transformation
of Sorting Products however, runs in the opposite direction, i.e. the
input Products are computed from the output products. To emphasize
this, we label the input products with |y|, as they are the result of
the transformation and the output products with |x|, as they are the
input to the transformation.

We use different symbols for Processes, Trees, Lists and Products: 

\begin{figure}[H]
\centering
\includegraphics[width=4cm]{ProductsSymbols.eps}
\caption{Symbols}
\end{figure}


\subsection{Unpack}

\begin{figure}[H]
\centering
\includegraphics[width=5cm]{ProductsUnpack.eps}
\caption{Unpack}
\end{figure}

What does an |unpack| process accept? It will accept containers which
bear a certain content and which carry certain labels. Note that our
notion of a container-label includes everything we know about the
container, including the container type.

But the content is not really determined by the unpack process
itself. Whatever it unpacks will be sent to some other process, which
also accepts only certain things. The unpack process itself can only
influence the containers it accepts\footnote{It may in theory choose
to accept \emph{less} content than its downstream processes, but that
is better handled by a dedicated operation and kept outside of mere
unpacking.}.

The container will sometimes be fully described by its label. This is
true if we know that the container must be empty once unpacked. But in
full generality, this is not the case.

\begin{note}

An Unpack process removes content from a container, such that both the
content and the remaining container statisfy certain criteria. It may
ask the container to be empty once unpacked, but it does not have to.

\end{note}

\begin{code}
pUnpack :: (IPredicate pred, Eq lty, Eq (pred lty)) =>
           Tree (pred lty) -> [Tree (pred lty)] -> Tree (pred lty)
pUnpack container content = 
        case container of
            Node cLabel cContent -> Node cLabel (L.union cContent content)
            Leaf cLabel          -> Leaf cLabel

-- Example container:
ex_container1 = Node (Labels ["Tray-BBZ1","Bag-BBZ1"])
                     [Leaf (Labels ["lint"])]
\end{code}

Once unpacked, the container above may still contain |lint| and hence
the unpack process accepts |lint|.

\begin{run}
       |*Main> pUnpack ex_container1 ex_plist1|
\perform{lpp $ pUnpack ex_container1 ex_plist1}
\end{run}

\subsection{Pack}

\begin{figure}[H]
\centering
\includegraphics[width=5cm]{ProductsPack.eps}
\caption{Pack}
\end{figure}

The Pack process is the dual of the Unpack process. It returns
information about the acceptable items and the acceptable
containers. This is all already specified by a downstream Unpack
process, so a Pack process has very little choice.

In the example from the Unpack process, a Pack process could use
containers labeled |Tray-BBZ1| or |Bag-BBZ1|. Without further
information, we cannot decide which of the two to use. 

Often, it may accept only empty containers, but nothing bad will
happen, if the container already contains something, provided that its
content is accepted by the downstream process. So a Pack process does
have a certain choice. In full generality, it can accept content both
as to-be-packed new material and as material which is already in the
container. Note that an empty container will always be accepted if it
has the right label.

\begin{note}
A |Pack| process adds content to a container. It computes an "item
product" and a "container product". In general the container does not
have to be empty.
\end{note}

\begin{code}
pPack :: (IPredicate pred, Eq lty) =>
         Tree (pred lty)  -> (Tree (pred lty), [Tree (pred lty)])
pPack (Node cLabel cContent) = (container, items)
        where
            container = Node cLabel cContent
            items     = cContent
-- unspecified content - not very useful:
pPack (Leaf cLabel) = (Leaf cLabel, [Leaf prAny])
\end{code}

\needspace{22em}
The following example shows, what a Pack process accepts, whose output
is |ex_container2|. The top part is the actual content and the bottom
part is the container. Note that the container may be pre-filled with
the same content that can be added to it.

\begin{code}
-- from the example above
ex_container2 = pUnpack ex_container1 ex_plist1
\end{code}

\begin{run}
       |*Main> pPack $ ex_container2|
\perform{lpp $ pPack $ ex_container2}
\end{run}

\subsection{Split}
\begin{figure}[H]
\centering
\includegraphics[width=5cm]{ProductsSplit.eps}
\caption{Split}
\end{figure}

A |Split| process accepts things, which are accepted by any of its
outputs. Each output may accept either a |ProdTree| or a |ProdList|.


The constructed Product |yi| is always a list. To compute it, we must
build the union over all the output products. 

\begin{code}
pSplit' :: (IPredicate pred, Eq lty, Eq (pred lty)) => 
          [Product (pred lty)] -> [Tree (pred lty)]
pSplit' ps = foldr L.union [] (map prodToList ps)
\end{code}

This is a somewhat crude implementation, because with our example
Product from earlier and this additional Product:

\begin{code}
ex_prod2 = ProdList
  [
    Node (Labels [ "xxx" ]) [ Leaf (Labels [ "foo1" , "foo2" ]) ],
    Node (Labels [ "bar" ]) [ Leaf (Labels [ "bar1" ]) ]
  ]

\end{code}

\needspace{22em}
We get

\begin{run}
       |*Main> pSplit' [ex_prod1, ex_prod2]|
\perform{lpp $ pSplit' [ex_prod1, ex_prod2]}
\end{run}

There is something to observe here: the "foo" and "xxx" Product
accept different labels, but accept the same content. Those two should
have been merged into one. Likewise the two "bar" products could be
merged, as they accept the same labels and their contents could be
merged into one. In fact, only the second "bar" Product is needed, as
it allows a "bar" to contain a "bar1", which always includes an empty
"bar".

To do better than that we need a smarter union operation. The core of
the problem is the following: A |Tree| is a pair-like thing of two
collections, as each node consists of a label-predicate and a
collection of children.

You could just as well list all the permissible combinations of
permissible Labels and permissible content. This would be the
cartesian product of the two collections. A Product would then have
the type

\begin{code}
data Product' lty = Product' lty (Product' lty)|
                    Leaf' lty

type ProductList lty = [Product' lty]
\end{code}

With real-world Products being |ProductLists|. However, |Product'| is
basically just a linked list, i.e. it is equivalent to

\begin{code}
type ProductList'' lty = [[lty]]
\end{code}

This is an outer list of inner lists, where each inner list tells you,
that if an item-label matches the head of the list, then all contained
items must match the tail of the list. If that is not the case, you
can try the next inner list until you exhausted the outer list.

This is the same thing as saying: an item labeled |a| inside a
container |b| inside a container |c| is accepted if the list |[c,b,a]|
is one of the inner lists

The outer list will in general be very large. To reduce its size, one
must compress it one way or another and the redundant entries in our
example were the result of suboptimal compression. There is a chapter
in the Appendices which elaborates on this issue.

\subsection{Merge / Restrict}
\begin{figure}[H]
\centering
\includegraphics[width=5cm]{ProductsMerge.eps}
\end{figure}

A simple, unconditional |Merge| simply reproduces its output product
on all its inputs, i.e. whatever its downstream process accepts is
accepted from all its inputs. In that respect |Merge| does not
transform Products at all. 

However, you may want to accept only a subset (e.g. priority mail) of
all acceptable things from one particular input and another subset
(e.g ordinary mail) from another.

But is it really the Merge process who decides that?  It appears that
these kind of restrictions are really first-class transformations, and
that they are not really tied to a Merge Process.

\begin{figure}[H]
\centering
\includegraphics[width=5cm]{ProductsRestrict.eps}
\caption{Restrict}
\end{figure}

So instead of |pMerge| we really need a |pRestrict|
transformation. Now, real-world restrict operations only check the
outermost labels and do not look inside containers. Checking outermost
labels is something an |IPredicate| does. So, the function we need
looks like this:

\begin{code}
pRestrict :: (IPredicate pred, Ord lty, Ord (pred lty)) => 
             (pred lty) -> Tree (pred lty) -> Tree (pred lty)
\end{code}


\begin{code}
pRestrict pred (Node lbl content) = toNone $ Node (prAnd pred lbl) content
        where
            toNone (Node x xs) 
                    | x == prNone = Leaf prNone
                    | otherwise   = Node x xs

pRestrict pred (Leaf lbl)         = Leaf (prAnd pred lbl) 

\end{code}

Remember the Tree we created with the Unpack process and which
accepted two labels?

\begin{code}
ex_tree3 = pUnpack ex_container1 ex_plist1
\end{code}

It looks like this:

\begin{run}
     |*Main> ex_tree3|\\
\perform{lpp ex_tree3}
\end{run}

\needspace{20em}
We can now restrict this product to a particular outermost label

\begin{run}
       |*Main> pRestrict (Labels ["Tray-BBZ1"]) ex_tree3|\\
\perform{lpp $ pRestrict (Labels ["Tray-BBZ1"]) ex_tree3}
\end{run}

Sometimes nothing is left over

\begin{run}
     |*Main> pRestrict (Labels ["punk"]) ex_tree3|\\
\perform{lpp $ pRestrict (Labels ["punk"]) ex_tree3}

\end{run}


There is however one restriction on content, which may be useful: We
may demand that a container must be empty. This cannot be expressed via a
predicate on lables anymore, but is a true function. Even when passed
a |Leaf|, i.e. a container where we originally did not care what's
inside, it returns a |Node|, because now we do care.

\begin{code}
restrictEmpty :: (IPredicate pred, Eq lty) => 
                 Tree (pred lty) -> Tree (pred lty)
restrictEmpty tree = Node (treeHead tree) []

\end{code}
\newpage
\section{Appendices} 
\input{Sop2}

\section{To do}
\begin{enumerate}
\item Describe unpack with respect to split.
\item Generalize aggegration. Currently this is a set of auxilary
      functions in Split. It should work on all pair-like things.
\item Implement ``priced'' Products, such that a sortplan can be
      generated
\item replace lists by sets where sequence is irrelevant and uniqueness is required
\item demonstrate the use of ranges instead of lists/sets
\item Provide a full real-world example
\item consider using Paths to describe Products and Items
\end{enumerate}

%\begin{figure}[H]
%\centering
%\includegraphics[width=4cm]{glass-slipper.jpg}
%\end{figure}

\end{document}
