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
\newmdenv[frametitle=Running it,backgroundcolor=gray!05,roundcorner=2pt]{run}

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

\title{Opal Concept}
\begin{document} \maketitle 

\begin{abstract}
\end{abstract}


\tableofcontents 
\listoffigures

%------------------------------------------------------------
\section{Goals}
%------------------------------------------------------------
The Goals of Opal can be summarized as follows

\subsection{Find optimal Production Plans}
\begin{itemize}
\item Minimize violations of Network-Planning input (e.g. bookings)
\item Minimize resource usage
\item Meet constraints
  \begin{itemize}
  \item do not use unavailable resources (personnel, machines,
    sortplans)
  \item don't send material to Processes which are incapable of
    processing it (don't send parcels to a letter sorting machine)
  \end{itemize}
\item Respond to a changing environment (allow easy re-planning)
\end{itemize}

\subsection{Report characteristics of Production Plans}
\begin{itemize}
\item Report the Production Plan itself, e.g. in the form of a
  timeline
\item Report the resource demands and the resource assignments
\item Report other ``bad things'' e.g. waiting times, excess resources
\item Report violations of Network-Planning input, particularly SLA
  violations
\item Report how a production plan changed over time, particularly
  planned vs. actual
\item Provide input to other planning systems, particularly Sorting
  Planning, Resource Planning and Network Planning (send re-bookings)
\end{itemize}
\subsection{Execute Production Plan}
\begin{itemize}
\item Communicate with external systems (``avoid the megaphone'')
\end{itemize}

%------------------------------------------------------------
\section{The World}
%------------------------------------------------------------

The |World| stands for what Opal needs to know about the world. This
includes:
\begin{description}
\item[ItemPositions] This is the position of each and every item Opal
  cares about
\item[ResourceAssignments] This is the current occupation and position
  of each and every resource.
\item[Pre-Announcements] This is an estimated future position of a set
  of items.
\item[Time] This is the current time of the world
\end{description}

\section{Items}

\subsection{Definition}

An |Item| is either a toplevel Item (an |ItemAt|) which is at a
certain |Position| or it is an Item which is inside a Container, where
the Container is an Item itself. The latter is called an |ItemIn|.

Items always belong to a |Product|. A Product is a classification of
Items, such that Items of the same Product are usually processed in
the same fashion. All letters for a given Postman can e.g. seen as
belonging to the same Product. For a single letter, the product would
be characterized by its destination (e.g by a Sortcode) and other
non-address attributes.

Finally we assume every items carries an Id.

\needspace{4em}
Hence the type of an Item depends of the type of the Ids |i| , the
type of the Products |p| and the type of the Positions |l|
(think: \emph{location}).

\begin{code}
import Data.List
import Data.Maybe
\end{code}
 
\begin{code}
data Item i p l = ItemAt i p l |
                  ItemIn i p i
                     deriving (Eq,Show)
\end{code}

\subsection{Operations}

We'll define some operations on Items, primarily predicates (tests)
and operations to move items around. Feel free to skip this section,
it is not essential and only needed to craft some examples later on.

\begin{code}

-- predicates

itemIs   :: (Eq i) => i -> Item i p l -> Bool
itemIsAt :: (Eq l) => l -> Item i p l -> Bool
itemIsIn :: (Eq i) => i -> Item i p l -> Bool

-- accessors

idOf :: Item i p l -> i

-- To determine the position of an item we must consider a whole set
-- of items, because we need to find the position of its container.

posOf :: (Show i, Eq i) => i -> [Item i p l] -> l

-- moving items

putItemAt :: l -> Item i p t -> Item i p l
putItemIn :: i -> Item i p t -> Item i p l
\end{code}

We omit the trivial implementations of these functions.

%if False
\begin{code}
itemIs id (ItemAt i p l) = i==id
itemIs id (ItemIn i p _) = i==id

itemIsAt loc (ItemAt i p l) = l==loc
itemIsAt loc (ItemIn _ _ _) = False

itemIsIn contId (ItemAt i p l) = False
itemIsIn contId (ItemIn _ _ i) = i == contId

putItemAt loc (ItemAt i p _) = ItemAt i p loc
putItemAt loc (ItemIn i p _) = ItemAt i p loc

putItemIn cnt (ItemAt i p l) = ItemIn i p cnt
putItemIn cnt (ItemIn i p c) = ItemIn i p cnt

idOf (ItemAt i _ _) = i
idOf (ItemIn i _ _) = i


posOf id items = case find (itemIs id) items of
  Just (ItemAt i p l)   -> l
  Just (ItemIn i p cnt) -> posOf cnt items
  Nothing               -> error ("Item " ++ show id ++ " not found")
\end{code}
%endif

\needspace{8em}
\subsection{Example}

With this humble definition we can simulate the unpacking of a
container.

The unpack operation takes an id (the container to unpack), a |dt|
parameter which is the time it takes to unpack a single item and an
initial state of the world.

The result shall describe how the world changes in the course of this
action, where the world is described by a |Time| and a collection of
Items. The result can thus be seen as a \emph{movie} of the world.

\begin{code}
exUnpack :: ExId -> ExTime -> ExWorld -> [ExWorld]

type ExId    = Int
type ExTime  = Int
type ExItem  = Item ExId String String
type ExWorld = (ExTime,[ExItem])


\end{code}

So here is the implementation:

\begin{code}
exUnpack id dt (t, items) = 
  case partition (itemIsIn id) items of
    ([],_)          -> [] -- No more Items in the container
    (i:is, others)  -> (t+dt, items') : exUnpack id dt (t+dt, items')
      where
        items'   = i' : (is ++ others)
        i'       = putItemAt pos' i 
        pos'     = posOf (idOf i) items
\end{code}

We create am example container and two Items which are inside the
container

\begin{code}
exItems :: [ExItem]
exItems = [
  ItemAt 1 "Container" "Area51",
  ItemIn 2 "Item" 1,
  ItemIn 3 "Item" 1
  ]
\end{code}

... and some helpers to print the result, whose implementation we omit in
this document.

\begin{code}
exPrint :: [ExWorld] -> String
exRun   ::  IO ()
\end{code}

%if False
\begin{code}
exPrint [] = "\n"
exPrint ((t, items):rest) = "t=" ++ (show t) ++ exPrintItems items
                            ++ "\n" ++ exPrint rest
  where
    exPrintItems is = concatMap (\i -> "\n\t" ++ (show i)) items

exRun = putStrLn $ exPrint $ initialState : exUnpack 1 10 initialState
  where
    initialState = (0, exItems)
\end{code}
%endif

When we run the example we see, that at each step a single item gets
unpacked and assumes the position of its container. At the end all
|ItemIn|-Items have been converted to |ItemAt|-Items.

\needspace{8em}
\begin{verbatim}
*Main> exRun
t=0
	ItemAt 1 "Container" "Area51"
	ItemIn 2 "Item" 1
	ItemIn 3 "Item" 1
t=10
	ItemAt 2 "Item" "Area51"
	ItemIn 3 "Item" 1
	ItemAt 1 "Container" "Area51"
t=20
	ItemAt 3 "Item" "Area51"
	ItemAt 2 "Item" "Area51"
	ItemAt 1 "Container" "Area51"

\end{verbatim}



Items and Resources share some traits insofar as mobile resources can
move around just like items, i.e. both Items and mobile Resources have
a |Position|.

%------------------------------------------------------------
\section{Reading from the real world}
%------------------------------------------------------------

In the previous example we did a simulation, which ran all by itself,
i.e. without any interaction with the real world. We must however, be
prepared that the world does not agree with our simulation. We expect
that the world is equipped with multiple sensors which can actually
look at the world and tell us what \emph{really} happened.

There are two types of information we expect to receive:

\begin{description}
\item[item-at] messages describe that an item was seen at a certain
  position. Such messages are typically triggered by scanning items.
\item[item-not-at] messages describe that an item is definitely
  \emph{not} at a certain postion or not inside a container. Such
  messages can be triggered by \emph{completion} events. An event
  \emph{unpacking completed} would tells us, that no items remain in
  the container.
\end{description}

The last case is particularly interesting. If a simulation believes an
item is in a container, but the world tells us it is not, where is it
then?

We can make better use of such messages, if the simulation is a bit
fuzzy. If the simulation does not just assume a single position of an
item, but several possible locations, then the information that an
item is no longer in a container allows us to at least rule out one
of the possibilities. Only when the simulation is \emph{certain} that
an item is at one and only one position, we are lost, when the world
tells us that this is not the case.

It is important to understand that \emph{unpacking completed} does not
tell us, that all items in the container are now at a postion where
the unpacking process put them. They could well have been moved away
while the unpacking was still in progress.

%\begin{figure}[htb!]
%\centering
%\includegraphics[width=4cm]{glass-slipper.jpg}
%\end{figure}

World = (ItemPositions, Resources, Time)

\end{document}
