%include lhs2TeX.fmt
%include greek.fmt
%options ghci -fglasgow-exts

\section{Items}

\subsection{Definition}

An |Item| is either a toplevel Item which is |at| a certain |Spot| or
it is an Item which is inside a Container, where the Container is an
Item itself. We call \emph{where the item is} its
|Location|\footnote{In earlier attempts I tried to model an Item as
  either a non-decomposable Item (an Atom) or an Item which contains
  other Items. It turned out that this is not so good. One of the
  problems was, that you could easily create Items which are in two
  containers at the same time. Also it is less \emph{relational} as
  the model we use here}.


Items always belong to a |Product|. A Product\footnote{In the Opal
  contexts products are called |Batch Declarations|} is a
classification of Items, such that Items of the same Product are
usually processed in the same fashion. All letters for a given Postman
can be seen as belonging to the same Product. For a single letter, the
product would be characterized by its destination (e.g by a Sortcode)
and other non-address attributes. There may also be Products for
|unsorted letters| and the like.

Finally we assume every items carries an Id.

\needspace{4em} Hence the type of an Item depends of the type of the
Ids |i| , the type of the Products |p| and the way we describe Spots
|s| \footnote{Shouldn't that be |Item i p l| where l is the location
  type? This would allow probalilistic Locations}. It would also
simplify |putItem|

%if False
\begin{code}
import Data.List
import Data.Maybe
\end{code}
%endif 

\begin{code}
data Location i s = At s | In i
                    deriving (Eq, Show)

data Item i p s = Item i p (Location i s)
                  deriving (Eq,Show)
\end{code}

Here is an example: we create a container and two Items which are
inside the container

\begin{code}
type ExId    = Int
type ExItem  = Item ExId String String

exItems :: [ExItem]
exItems = [
  Item 1 "Container" (At "Area51"),
  Item 2 "Item" (In 1),
  Item 3 "Item" (In 1)
  ]

\end{code}

\subsection{Operations}

We'll define some operations on Items, primarily predicates (tests)
and operations to move items around. Feel free to skip this section,
it is not essential and only needed to craft some examples later on.

\begin{code}
-- predicates
itemIs        :: (Eq i) => i -> Item i p s -> Bool
itemIsLocated :: (Eq s, Eq i) => (Location i s) -> Item i p s -> Bool

-- accessors
idOf :: Item i p s -> i

-- To determine the Spot of an item we must consider a whole set
-- of items, because we need to find the Spot of its container.
spotOf :: (Show i, Eq i) => i -> [Item i p s] -> s

-- moving items
putItem :: (Location  i s) -> Item i p l -> Item i p s
\end{code}

We omit the trivial implementations of these functions\footnote{The
  implementation is just hidden in this document. It is still in the
  source file and this specification remains executable}.

%if False
\begin{code}
itemIs id (Item i _ _) = i==id
itemIsLocated loc (Item i p l) = l==loc
putItem loc (Item i p _) = Item i p loc
idOf (Item i _ _) = i


spotOf id items = case find (itemIs id) items of
  Just (Item i p (At l)) -> l
  Just (Item i p (In c)) -> spotOf c items
  Nothing                -> error ("Item " ++ show id ++ " not found")
\end{code}
%endif

\needspace{8em}
\subsection{Example}

With this humble definition we can simulate the unpacking of a
container.

The unpack operation takes an id (the container to unpack), a |dt|
parameter which is the time it takes to unpack a single item and an
initial state of the world.

\ding{228} The result shall describe how the world changes in the
course of this action, where the world is described by a |Time| and a
collection of Items. The result can thus be seen as a \emph{movie} of
the world.

\begin{code}
exUnpack :: ExId -> ExTime -> ExWorld -> [ExWorld]

type ExTime  = Int
type ExWorld = (ExTime,[ExItem])
\end{code}

So here is the implementation:

\begin{code}
exUnpack cnt dt (t, items) = 
  case partition (itemIsLocated (In cnt)) items of
    ([],_)          -> [] -- No more Items in the container
    (i:is, others)  -> (t+dt, items') : exUnpack cnt dt (t+dt, items')
      where
        items'   = i' : (is ++ others)
        i'       = putItem (At loc') i 
        loc'     = spotOf (idOf i) items
\end{code}

\needspace{8em}
Some helpers to print the result, whose implementation we omit in
this document.

\begin{code}
exPrint :: [ExWorld] -> String
exRun   ::  ExWorld -> IO ()
\end{code}

%if False
\begin{code}
exPrint [] = "\n"
exPrint ((t, items):rest) = "t=" ++ (show t) ++ exPrintItems items
                            ++ "\n" ++ exPrint rest
  where
    exPrintItems is = concatMap (\i -> "\n\t" ++ (show i)) items

exRun initialState = putStrLn $ exPrint $ initialState : exUnpack 1 10 initialState
\end{code}
%endif

When we run the example we see, that at each step a single item gets
unpacked and assumes the Spot of its container. At the end no more
items as |In| a container.

\needspace{8em}
\begin{verbatim}
*Main> exRun (0,exItems)
t=0
	Item 1 "Container" (At "Area51")
	Item 2 "Item" (In 1)
	Item 3 "Item" (In 1)
t=10
	Item 2 "Item" (At "Area51")
	Item 3 "Item" (In 1)
	Item 1 "Container" (At "Area51")
t=20
	Item 3 "Item" (At "Area51")
	Item 2 "Item" (At "Area51")
	Item 1 "Container" (At "Area51")
\end{verbatim}

\subsection{Performance}

Our simple list-based implementation shows very poor
performance. Unpacking a container with 1000 Items already takes close
to one second. 3000 Items already take several seconds and execution
time increases non linearly. This is not too amazing, since we
\begin{itemize}
\item do a full table scan over all the items with each unpack
  operation
\item keep all the world states around such that unpacking 1000 items
  leads to 1000 world states.
\end{itemize}

Since \emph{premature optimization is the root of all evil} (Knuth,
Donald 1974), we leave things as they are for now, particularly since
the solutions to the above problems appear obvious (use
hashes/indexes, discard unneeded world states).

