\documentclass[a4paper]{article}
%include lhs2TeX.fmt
%include greek.fmt
%options ghci -fglasgow-exts
%format union = unio n

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


\tableofcontents 
\listoffigures
%if false
\begin{code}
{-# Language FlexibleInstances #-}
{-# Language FlexibleContexts #-}
{-# Language StandaloneDeriving #-}
{-# Language UndecidableInstances #-}
{-# Language RankNTypes #-}
{-# Language ConstraintKinds #-}

import qualified Data.List as L
import qualified Data.Set as S
import Data.Monoid hiding(Product)
import Data.Ord
import qualified Data.Foldable as F
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
many more. They are described by three features

\begin{description}
\item[The item label] which in our world describes everything we need
  to know about the item, whether or not this information is
  physically printed on a piece of paper.
\item[Containment information], i.e. information about what other
  items are contained in an item, or alternatively in which item a
  given item is currently contained.
\item[Choice], i.e. the fact that more than one item lies in the
  focus, or that more than one item is acceptable.
\end{description}

In the real world almost everything contains something else, so the
chain of containemnt would never end. In our world however, we often
reach a point, where we just won't look any further. A parcel is
certainly a container and it does contain other items, however, a
parcel processing company will not care about what is in the parcel.

The only situation I can think of, where this chain of containemnt
ends, is that eventually we expect items to be completely empty. If we
can model this correctly, we are not only able to reason about empty
containers, but also about empty containers being contained in bigger
(then nonempty) containers.

\needspace{22em}
\subsubsection{Item labels}
\begin{note}
The label stands for everything we need to now about the item,
including things like the container type.
\end{note}

The weight of a parcel may technically not be printed on a label, but
we treat it as part of the label nontheless. We make no assumption
about the label format, i.e. whether an item is characterized by
Format, Class and Destination or by anything else.

Note that the label-type |lty| must be general enough to describe all
labels in our universe. We cannot e.g. define seperate label types for
trays and letters. In practice, this is not a limitation, because
different label types can always be united under one union-type.

\subsubsection{Containement}

There are two ways to model containment:

\begin{description}
\item[outside-in] means describing an item by its label and what other
  items it contains.
\item[inside-out] means describing an item by its label, the label of
  its container and so forth.
\end{description}

We chose the second approach for reasons I cannot fully explain. The
outside-in approach does not allow to model a single item, once it is
in a container with other items, while the inside-out approach has no
difficulties with this. Hence, doing it inside-out seemed more
\emph{fundamental} to me.


To describe containment inside-out, we basically need a list, where
|[1,2,3]| would describe an item, labeled |3| inside an item, labeled
|2| inside an item, labeled |1|. 

Now we still need to be clear about whether or not item |3| is empty
or if we just don't know what's inside. We call this the |Ending| of
the containment and there are exactly two options:

\begin{code}
data Ending = Open | Closed 
              deriving (Eq, Show)
\end{code}

Where |Open| means that we don't know or care what's inside and
|Closed| means that the containment is fully described, i.e. item |3|
contains no further items.

\begin{note}
We know precisely what is inside an empty container, namely nothing.
\end{note}


A single, potentially "wrapped" item is then described by a list of
labels, plus this ending indicator.

\begin{code}
data Wrapped lty = Wrapped Ending [lty] 
                   deriving (Eq, Show)
\end{code}

A |Wrapped| can already describe a Set, because

\begin{itemize}
\item The label information may not reflect all information on the
  physical item of an item. Particularly we may not include the
  item|Id|. Thus a single |Wrapped| may stand for many physical items.
\item In an |Open| Wrapped we do not know what's inside the innermost
  item.
\end{itemize}

In any case, we need an operation which tells us, whether one
|Wrapped| \emph{is} included \emph{in} another one.
\begin{code}
isIn (Wrapped _ _ ) (Wrapped Open []) = True
isIn (Wrapped e xs ) (Wrapped Closed []) = 
        null xs
isIn (Wrapped e1  (a:as)) (Wrapped e2  (b:bs)) =
        (a == b) && 
        Wrapped e1 as `isIn` Wrapped e2 bs
isIn _ _ = False
\end{code}

When you look at the first line, you'll realize that |Open []| matches
anything. This gives rise to the definitions:

\begin{code}
anyWrapped = Wrapped Open []
noWrapped  = Wrapped Closed []
\end{code}

A |Wrapped| can be ordered, provided that the label-type |lty|
can be ordered. We chose an ordering such that shorter |Wrapped| come
after longer |Wrapped| and |Open| comes after |Closed|, i.e. the most
general value comes last.


\begin{code}
instance Ord Ending where
        compare Open Closed = GT
        compare Closed Open = LT
        compare _ _         = EQ

instance (Eq lty, Ord lty) => Ord (Wrapped lty) where
        -- sorts most general value last
        compare (Wrapped e1 as) (Wrapped e2 bs) = 
                compare (Down as) (Down bs) <> 
                compare e1 e2 

-- xxx useful?
instance Functor Wrapped where
        fmap f (Wrapped ending xs) = Wrapped ending (fmap f xs)

\end{code}

\begin{run}
|*Main> L.sort [Wrapped Open [1,2], Wrapped Closed [1,2,3], |\linebreak\ |                     Wrapped Open [1,2,3]]|
\perform{lpp $ L.sort [Wrapped Open [1,2], Wrapped Closed [1,2,3], Wrapped Open [1,2,3]]}
\end{run}

\subsubsection{Choice}

The next thing we need to do, is to extend the simple concept of
wrapped items to collections of these. Now there are many
possibilities to model a collection (a List beeing the most obvious
one) and we do not want to choose one upfront. However, we do know
that a collection must be Set-like, i.e. it must support operations
for union, intersection and for putting a single |Wrapped| into a
collection.

\begin{code}
class Wset s where
        union :: (Ord a) => s a -> s a -> s a
        inter :: (Ord a) => s a -> s a -> s a
        singl ::            Wrapped a  -> s a
\end{code}


A List has the disadvantage, that it can contain the same value more
than once. However, if we only maninipulate the List with the |Wset|
operations, we can avoid this. Here is the implementation of a Wset by
means of a List:


\begin{code}
data WrappedList lty = WrappedList [(Wrapped lty)] 
                       deriving (Eq, Show)

instance Wset WrappedList where
        union = wlUnion
        inter = wlIsect
        singl x = WrappedList [x]
\end{code}

\needspace{12em}
\begin{code}
wlUnion (WrappedList as) (WrappedList bs) = WrappedList $ fst ys : snd ys
        where
            ys = foldr f (Wrapped Closed [], []) (L.sort (as ++ bs))
            f cx (cacc, yacc)
                    | cx `isIn` cacc = (cacc, yacc)
                    | otherwise      = if cacc == Wrapped Closed [] 
                                       then (cx, yacc)
                                       else (cx, cacc:yacc)


wlIsect (WrappedList as) (WrappedList bs) = WrappedList (isect as bs)
        where
            isect _ [] = []
            isect [] _ = []
            isect (x:xs) (y:ys) 
                    | x `isIn` y = x : isect xs (y:ys)
                    | y `isIn` x = y : isect (x:xs) ys
                    | x > y      =     isect xs (y:ys)
                    | x < y      =     isect (x:xs) ys

\end{code}

Examples:
\begin{code}
ex_l1 = singl (Wrapped Closed [1,2,3]) :: WrappedList Int
ex_l2 = singl (Wrapped Open [1,2,4])   :: WrappedList Int
ex_l3 = singl (Wrapped Open [1,2])     :: WrappedList Int
ex_union1 = ex_l1 `union` ex_l2 `union` ex_l3
\end{code}

\begin{run}
|*Main> ex_union1|
\perform{lpp $ ex_union1}
\end{run}

You see, only the most general "item" survived, as it contains the
other two.


\subsubsection{The Items data type}

The |Items| data type stands for a collection of wrapped items. It is
simply a type synonym, which depends on the the way we represent
|Nset| and the label-type |lty|.


\begin{code}
type Items set lty = (Wset set) => set lty
\end{code}

\subsection{Products}

A Product is something which can answer, whether it accepts a given
Item. However, there is no \emph{isElement} function available in the
typeclass.

Such a function is easy to write. We simply must convert the element
into a singleton set (by means of |singl| and then check if this is a
subset of the |Wset| using |inter|. Sort of like

\begin{eqnarray}
a \in M \Leftrightarrow \{a\} \cap A = \{a\}
\end{eqnarray}


  To check whether a
|Wrapped| item is part of a Wset, we create a singleton set and
compute the intersection with the Wset and expect to get the singleton
back.

\begin{code}
element :: (Eq (set lty), Ord lty, Wset set) => 
           Wrapped lty -> set lty -> Bool
element item set = let si = singl item
                   in si == inter set si
\end{code}

\begin{run}
|*Main> ex_union1|\\
  \eval{ex_union1}

|*Main> Wrapped Closed [1,2,3] `element` ex_union1|\\
  \eval{Wrapped Closed [1,2,3] `element` ex_union1}
\end{run}


However, labels of Items tend to be more detailed than the lables
which form a Product. Particularly an Item may carry an |id|, which
often plays no role in deciding whether or not it is accepted by a
Product.

This means, we may have to ignore certain parts of an Item label in
order to match it with a Product. It also means that the label-types
of Items and Products may differ, but we must use the same label-type
for all Items and the same label-type for all Products. And there must
be an operation which projects any Item-Label to a Product-Label.

Other than that, there is not much to say about Products. They
essentially look like Items with a less detailed label-type.


\subsubsection{The Product data type}

So a |Product| data type is just a synonym, which depends on the
the way we represent |Nset| and the label-type |lty|.

\begin{code}
type Product set lty = set lty
\end{code}

Evetually we'll have to reveal the |Wset| implementation, but it is
sufficient to do this at the very end of a computation.

\begin{code}
ex_p1 = singl (Wrapped Closed [1,2,3]) 
ex_p2 = singl (Wrapped Open [1,2]) 
-- reveal |Wset| implementation
ex_union2 = ex_p1 `union` ex_p2 :: Product WrappedList Int
\end{code}

\begin{run}
       |*Main> ex_union2|
\perform{lpp $ ex_union2}
\end{run}


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
-- pUnpack :: Ord lty => WrappedList lty -> WrappedList lty -> WrappedList lty
pUnpack (WrappedList containers) (WrappedList items) = (union)  (unions items') (WrappedList containers)
        where
            items' = do
                (Wrapped e1 cs) <- containers
                (Wrapped e2 is) <- items
                return $ singl (Wrapped e1 (head cs : is)) -- xxx do OPEN CLOSED right

-- pp $ (pUnpack (singl (Wrapped Open [2])) (singl (Wrapped Open [1,2])) :: (WrappedList Int))




unions :: (Wset set, Ord lty) => [set lty] -> set lty
unions xs = foldr union (singl noWrapped) xs

-- xxx why is it so difficult to look inside. All I can see is lty but not the wrapped.

\end{code}

\end{document}
