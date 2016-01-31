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


\tableofcontents 
\listoffigures
%if false
\begin{code}
{-# Language FlexibleInstances #-}
{-# Language FlexibleContexts #-}
{-# Language StandaloneDeriving #-}
{-# Language UndecidableInstances #-}

import qualified Data.List as L
import qualified Data.Set as S
import qualified Data.Monoid as M
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
many more. They are described by two features

\begin{description}
\item[The item label] which in our world describes everything we need
  to know about the item, whether or not this information is
  physically printed on a piece of paper.
\item[Containment information], i.e. information about what other
  items are contained in an item, or alternatively in which item a
  given item is currently contained.
\end{description}

In the real world almost everything contains something else, so the
chain of containemnt would never end. In our world however, we often
reach a point, where we no longer care about containment. A parcel is
certainly a container and it does contain other items, however, a
parcel processing company will not care about what is in the parcel.

There are however cases, where we don't hit a ``don't care'' situation
when we look deeper and deeper into containers. The only situation I
can think of, is that eventually we expect items to be completely
empty. If we can model this correctly, we are not only able to reason
about empty containers, but also about emopty containers being
contained in bigger (then nonempty) containers.

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
\item[inside-out] means describing an item by its label and what other
  items it is \emph{contained in}.
\end{description}

We chose the second approach for reasons I cannot fully explain, say
it was a gut-feeling. Grouping items which are inside the same
container seems like a subsequent operation and this grouping should
not be part of the |Item| data type as such. Doing it inside-out
seemed more \emph{fundamental} to me.

To describe containment outside-in, we basically need a list, where
|[1,2,3]| would describe an item, labeled |3| inside an item, labeled
|2| inside an item, labeled |1|. 

Now we still need to be clear about whether or not item |3| is empty
or if we just don't know what's inside. We call this the |Ending| of
the containment and there are exactly two options:

\begin{code}
data Ending = Open | Closed deriving (Eq, Show)
\end{code}

Where |Open| means that we don't know or care what's inside and
|Closed| means that the containment is fully described, i.e. item |3|
contains no further items.

\subsubsection{Compression}

There is a third aspect, which has something to do with the
representation of items and e.g. the way |SOP| encodes items. When
there are several option at each level of nesting, describing a set of
Items is like computing the cartesian product of all those options. If
e.g. we have containers labeled 1,2 or 3 each containing items labeled
10,11 or 12, then the whole set of items contains 9 entries. This is
somehwat wasteful and blurrs the nature of this set of items.

So instead of encoding an item as a list of labels, we encode items as
a list of list-of-labels. The item above could then be written as 

|Open [[1,2,3], [10,11,12]]|

Now the containment information is clearly a list, because the
ordering of elements is significant. However, the inner lists, which
describe the various options for a label in that position just have to
be set-like. This:

|Open [[3,2,1], [11,11,10]]|

describes exactly the same items as before, whereas this:

|Open [[10,11,12], [1,2,3]]|

swaps containers and content, which is a totally different set of
items.

\subsubsection{The Items data type}

If we put all these considerations together, we end up with a
definition for a set of items, which can be described by a cartesian
product.

\begin{code}
data Nested grp lty = Nested Ending [grp lty] 
                 deriving (Eq, Show)
\end{code}

So a |Nested| is something which has an |Ending| and otherwise
consists of a List of groups-of-labels. The list expresses the
containment information, with the head of the list being the outermost
item. Here |grp| stands for anything set-like, which we use to express
groups of labels.

In general a set of Items cannot be described in this fashion
alone. Not everything can be written as a cartesian product. So we
need another level of manyfication, such that we can say a set of
Items is a set of |Nested|. Again we are free to chose the
representation of |set| and it does not have to be the same
representation we used to describe alternative labels. We call this
second manyfication |ors| as it desribes an |or| condition.

\begin{code}
data Items ors grp lty = Items (ors (Nested grp lty)) 
                       
deriving instance Show (ors (Nested grp lty)) => Show (Items ors grp lty)

\end{code}

So we essentially wrapped the |Nested| type we already had into |ors|,
which basically means, that |Items| stands for a collection of Items
and is expressed as a bunch of |Nested|.


\subsubsection{Uncompressing}

The only reason why we used a |grp| to group alternatives labels is
space-efficiency and to pave the road for SOP-like operations and
eventually write compact sortplans. 

We can write an operation with undoes the grouping. We still need to
carry the |Ending| information around.

Being lazy be write this for lists first
\begin{code}
-- cartesian product of lists of lists
cartesian :: [[a]] -> [[a]]
cartesian [] = [[]]
cartesian (x:xs) = do
    a <- x               -- a  :: lbl
    as <- cartesian xs   -- as :: [lbl]
    return (a : as)
\end{code}

Then we just convert our |Nested| into lists. 

\begin{code}

-- Every foldable can be converted into a List. 
fToList :: (F.Foldable f) => f a -> [a]
fToList fas = F.foldr f [] fas
        where
            f a xs = a:xs


-- cartesian uncompression of a |Nested|
nCartesian :: F.Foldable grp => Nested grp lty -> [(Ending, [lty])]
nCartesian (Nested ending lbls) = map toNested $ cartesian (map fToList lbls)
        where
            toNested xss = (ending,  do
                               x <- xss
                               return x
                           )
\end{code}

\begin{run}
       |*Main> nCartesian $ Nested Open [[1,2],[3,4]]|
\perform{lpp $ nCartesian $ Nested Open [[1,2],[3,4]]}
\end{run}



\subsection{Products}

A Product is something which can answer, whether it accepts a given
Item. Since Items are already let-like, Products could be modeled in
exacly the same way. A Product would accept an Item if the Items (or
more precisely |Items| containing just this one item) is a subset of
the Product.

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

\subsection{Partial Orders and Subsets}

We used the term |set-like| quite a bit. What we mean by this, is
something that implements a |subset| relationship. |Subset| again is
an example of a |partial order|, this is a binary relation, which is
reflexive, transitive and antisymmetric. So |set-like| and partial
order mean the same thing.

When we use |<:| as the comparison operator (think: subset), the a partial order is:

\begin{code}
class PartialOrder po where
        (<:) :: (Ord a) => po a -> po a -> Bool
\end{code}

The two manyfications we used earlier have two obvious
implementations, namely |Set| and |List|. Both are partial orders:

\begin{code}
instance PartialOrder S.Set
        where
            (<:) = S.isSubsetOf


instance PartialOrder []
        where
            as <: bs = (L.sort as) `L.isPrefixOf` (L.sort bs)
\end{code}

Our |Nested| data type is a partial order itself, if it uses a partial
order for the |grp| manification.

\begin{code}
instance (PartialOrder grp) => PartialOrder (Nested grp) 
        where (<:) = contains

contains :: (PartialOrder grp, Ord a) => Nested grp a -> Nested grp a -> Bool

contains (Nested _ _ ) (Nested Open []) = True
contains (Nested e xs ) (Nested Closed []) =
        e == Closed && 
        null xs
contains (Nested e1  (a : as)) (Nested e2  (b : bs)) =
        (a <: b) && (Nested e1 as) `contains` (Nested e2 bs)
contains _ _ = False

\end{code}

Our |Items| data type is also a partial order but it is more difficult
to write. The problem is, that |Items| are basically a set of
cartesian products and the question whether or not one set of
cartesian products is a subset of another such set is not that easy to
answer. 






\end{document}
