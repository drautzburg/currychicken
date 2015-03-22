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
\title{XXX}
\begin{document} \maketitle 
%if False
\begin{code}
{-# LANGUAGE RankNTypes #-}
import qualified Data.List as L
import Prelude hiding (filter)
\end{code}
%endif


\begin{abstract}
\end{abstract}


\tableofcontents 
\listoffigures

\section{Introduction}
This document addresses a number of issues related to simulating
Material flows.

\subsection{Equal rights for consumers and producers} 

When Material flows through a network, you may mentally follow the
flow of Material. Processes would accept Material and produce new
Material. A Process woul primarily be desrcibed by the way it
transforms Material.

You may however just as well make the \emph{willingness to accept}
Material a first class citizen. In the postal world you would start
e.g. with your private mailbox, which accepts letters and flats up to
maximum amount. The mailman transforms this ``service'' to a more
valuable service, because he accepts mail for an entire route. An on
and on it goes until you have fabricated the service the postal
operator offers as a whole, namely ``from anywhere to anyone'' (Canada
Post slogan).

A computer model which favors one perspective over the other exhibits
an element of arbitrariness, while a model which gives producers and
consumer equal rights appears much more natural.

\subsection{Flow based simulation}

A discrete event simulation (DES) which models each and every item is
conceptually simple. But it is computationally expensive and has a
number of semantic problems.

\begin{description}
\item[Concretizing input data:] You may not know about each and every
  item which enters your network, but you may only have ballpark
  information. A DES which is based on individual items would have to
  ``make up'' items from this. This is another element of
  arbitrariness. 
\item[Un-randomizing] Many processes will exhibit random behavior. If
  you pick an item from a pile of mail, you cannot possibly know which
  item will be the chosen one. You only really know that ``one of n''
  got picked. An item-based DES however, would have to chose a
  concrete item. Even if this is a random choice, the randomness is
  concealed. It appears like the simulation knows what item to pick,
  where in reality it does not.
\end{description}

\subsection{Describing Material}

While in some cases Material is known on a per-item basis, you often
have only vague information. A good simulation should be able to
handle any Material description, vague or concrete.

Material comes in two varieties. First as physical (producible)
Material and second as accepted (consumable) Material. A simulation
needs to be able to do operations on either variety and to relate
producible to consumable Material. Basically it needs to be able to

\begin{itemize}
\item Compute the effect of merges and splits on consumable and
  producible Material.
\item Compute the resulting flow, when the consumable and producible
  Material is known.
\end{itemize}

\subsection{Resources}

Processes not only produce and consume Material, they also need
resources to do their job. When you split mail by means of a sorting
machine, the following conditions affect the process:

\begin{itemize}
\item Material must be available at the input (feeder).
\item There must be room in the output buffers (stackers).
\item The machine itself must be available.
\item Staff to operate the machine must be available.
\end{itemize}

If any of these conditions is not met, the process will not be able to
do anything.

Traditionally Material is treated as one thing and resources as
another. However, this is an arbitrary choice. Staff circulates within
a hub just like Material and there are processes which distribute
staff over jobs. 

Unlike Staff and Material, a sorting machine does not move. Hence it
feels strange to ``send a sorting machine to a job''. But it does not
take much abstraction to make this a perfectly valid view.

Then a Process (or a job, or a workorder, whatever you call it)
becomes the center of attention. Material, staff and other resources
are consumed and produced by Processes. A Resource Pool acts like a
buffer for resources. Everything flows.

\subsection{Separating predictions from amendments}

When a truck arrives and you need to unload that truck, you shoud make
sure staff is availble. It does not make much sense to send staff too
early (before the truck arrives), because then your employess would
have to sit and wait. Likewise it does not make much sense to send
staff too late, because then the truck is wasting time.

Recognizing the situation where either the truck or the staff is
waiting is one thing and amending the planning, such that this
situation is avoided is another. It appears desirable to completely
separate these two aspects.

Making the prediction is primarily a technical problem. However
amending may require true intelligence. As an analogy consider a
project which is predicted to miss its deadline. Making this
prediction is often quite easy. However, deciding what to do to avoid
this desaster is quite difficult.

In a planning system, amendments can be entirly left to the user. She
will still have the benefit of knowing what she will get herself into,
because the system is able to make predictions. In some cases, as in
the arriving truck example, the amendment may be obvious and can be
automated. In general, we can enhance the system step-by-step by
implementing more and more automatic ammendments.

A by-product of this separation is the ability to respond to future
situation. You can e.g. easily start a process some time \emph{before}
some situation occurs. Had we not separated predictions and
ammendments we would have gotten into deep trouble with the flow of
time. You would have

\begin{description}
\item[The wallclock time] i.e. the time of the real world. It only
  flows in one direction and you cannot rewind it.
\item[The simulation time] i.e. the time a simulation thinks
  something will happen. This time always lies in the future.
\item[The planning time] i.e. the time at which a planner decided
  something shall happen. This time does not have a direction, you can
  plan things in any order, something for tomorrow, something for the
  next week and then something for the day after tomorrow.
\end{description}

Due to this separation, the simulation only takes planned events
(which are based on planning time) as input. It would never alter the
planning itself.

There are some drawbacks though. You can no longer include things like
``unload the truck as soon as it arrives'' in your planning. You can
only say ``unload it at 08:15''. Knowing the futility of unloading a
truck which isn't there is thus pushed to a higher level of the
software. Still I believe the benefits of this separation outweigh its
drawbacks.

\subsection{Detecting bad things}

While a simulation produces a ``movie'' of future system states, you
are typically not interested in every scene of the movie. A planner is
only intersted in certain aspects, particularly whether or nor bad
things are expected to happen.

Hence a simulation needs an afterburner, which condenses the series of
future system states into something which is intersting for a planner
to know.

Ideally the simulation would record what a planner wants to know ``as
it goes''. This would be a lot more efficient than recording
``everything'' and later compute KPIs and other interesting stuff.


\section{Concepts}

% ------------------------------------------------------------
\section{Material Production and Consumption}

If you position youself on an edge in a network, you will see an
upstream side and a downstream side, where \emph{up} and \emph{down}
refers to the flow of material.

The upstream side produces material and the downstram side provides a
service to consume material and the combination of material production
and the consumption service determines the resulting flow.

\begin{itemize}
\item If the upstream side does not produce any material, nothing will
  flow.  
\item But also if the the downstream side is not willing to consume
  any material, nothing will flow.
\item If the downstram service consumes all produced material, then
  changing the downstream side to consume even more material will not
  lead to any change in the resulting flow.
\item Likewise when material production equals or exceeds material
  consumption, producing even more material will not change the
  resulting flow. 
\end{itemize}

In a way this corresponds to the saying that a chain is only as strong
as its weakest link.

No matter how we describe production and consumption, there has to be
an operation which computes a resulting Flow. This operation has some
resemblence with a $min$ operation.

\begin{code}
mmin :: Production -> Consumption -> Flow
mmin = undefined
\end{code}



\subsection{Set-operations}

Material behaves a lot like sets. Particularly a \emph{merge} Process
perform a \emph{union} operation. Material can be counted which
corresponds to the \emph{cardinality} of a set.

There are two aspects which may require customization:

\begin{itemize}
\item The way we characterize the elements in a set, i.e. the elements
  of Material may differ from application to application. Elements may
  be described by destination and class or a combination of other
  attributes. We use the letter \verb!e! to represent the type of
  elements of the set
\item The way we represent sets as such. Sets can be implemented as
  lists, hashes, trees and many more. We use the letter \verb!s! for
  that.
\end{itemize}

We want the main simulation to be unaware of these implementation
aspects. In an OO world, you may try to achieve this with
subclassing. Here we reason as follows.

There are a number of set-operations which depend on the
representation we choose for sets.

\begin{code}
data SetOps s = SetOps {
  sCount    :: forall e.                  s e -> Int,
  sFilter   :: forall e.                  (e -> Bool) -> s e -> s e,
  sAdd      :: forall e. (Eq e, Ord e) => s e -> s e -> s e,
  sFromList :: forall e.                  [e] -> s e
}
\end{code}

In case we represent Sets as Lists, the SetOps can be implemented in
terms of existing List-operations.

\begin{code}
listSetOps :: SetOps []
listSetOps = SetOps {
               sCount    = L.length,
               sFilter   = L.filter,
               sAdd      = \x y -> L.nub $ L.sort $ (x ++ y) ,
               sFromList = id
           }
\end{code}

Example

\begin{code}
exSet1 = sFromList listSetOps [1..4]
exSet2 = sFromList listSetOps [2..6]
\end{code}

|*Main> sFilter listSetOps odd $ sAdd listSetOps exSet1 exSet2|\\
  \eval{sFilter listSetOps odd $ sAdd listSetOps exSet1 exSet2}


\section{Processes}

Processes do two things
\begin{itemize}
\item they transform input Material to output Material
\item the transform used Services to provided Services
\end{itemize}


\begin{code}

\end{code}





%\begin{figure}[htb!]
%\centering
%\includegraphics[width=4cm]{glass-slipper.jpg}
%\end{figure}

\end{document}
