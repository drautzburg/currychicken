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
\title{Resolving ambiguities in Sorting Products}
\begin{document} \maketitle 

\begin{abstract}

When constructing Sortplans the planner needs to make sure that every
separation only receives mail which is accepted by a downstream
process. However, it may happen that for a given mailpiece there is
more than one possible downstream process. In that case a decision
must be made, which of the possible processes is \emph{best suited}
for this mailpiece.

This paper discusses this problem.
\end{abstract}


\tableofcontents 
\listoffigures

\section{Introduction}
\subsection{Sorting}

Sortplans define how to split up mail into a number of
separations. Such a process is needed, because each separation is
destined for a downstream process and those downsream processes are
typically more specialized and do not accept any mailpiece. At the end
of this process chain you may picture your private mailbox-slot, which
only accepts mail which is addresses to your or your family.

Somewhere mail must be split up into separations as fine-grained as
mailbox-slots. This work is typically done by a postman.

An ambiguity may already occur at that level. An ambiguity may occur
e.g. when the postman is free to deliver a parcel to your
neighbour. Obviously both you and your neighbour accept parcels which
are destined to your address.

Still delivering the parcel directly to you is the preferred
solution. This is because it saves additional processing, namely to
transport the parcel from your neighbour to you.

However, this problem cannot easily be solved with a Sortplan. This is
because the sortplan creator would have to know whether or not you're
at home at the time of the delivery attempt. But, for the sake of
argument, let's assume that this information is available.

If either you or your neighbour (but not both) are are home, then
there is no ambiguity. These is only one possible delivery point. 

If neither you, nor your neighbour are at home, then the postman
cannot deliver the item. He should not have accepted it in the first
place, because it will go back to the depot anyways.

If both you and your neighbour are at home, then delivering the item
to you is the \emph{cheaper} solution. The disambiguation will be done
by choosing the cheapest option.

This is the general idea: whenever there is ambiguity, the cheapest
solution will be chosen. This requires a \emph{price model} to be
built into the system which constructs sortplans.

It should be noted, that the optimal solution is a solution for
\emph{disposing} things.

\subsection{Classification}

Sorting machines today don't take decisions based on individual
items. Instead they read the address and other non-address-attributes,
e.g. \emph{priority} and compute a classification of the mailpiece. In
a second step they decide where \emph{all} mailpiceses of that
classifcation shall go.

Suppose a mailpice can be described by a number of attributes which
together can be summarized in some type |mpc|. Also suppose that there
are a number of classifications which can be summarized in a type
|cls|. Then a classification is a mapping from |mpc| to |cls|.


\begin{code}
type Classification cls mpc =  mpc -> cls
\end{code}

To take a sorting decision, the sorting machine takes a
classification |cls| and looks it up in a \emph{Sortplan}. The result
of that lookup is an output |out|.

\begin{code}
type SplLookup out cls = cls -> out 
\end{code}

The entire sorting decision maps a mailpice to an output

\begin{code}
type Split out mpc = mpc -> out
\end{code}

and it can be constructed from a classification and a Sortplan lookup

\begin{code}
split ::  Classification cls mpc -> SplLookup out cls -> Split out mpc
split classification spLookup = spLookup . classification
\end{code}

For the Disambiguation problem we are only concerned with the
|SplLookup|, i.e. we are essentially trying to find a way to construct
a Sortplan.


\subsection{The buying analogy}

We can construct an analogy to the disambiguation of sorting by
turning everything around and look for an optimal way of buying
things. We put ourselves in a position, where we want to construct
items of a certain product. The question is where to buy the component
parts from.

Instead of the downsream processes we now have to consider upstream
vendors. As long as there is only one vendor for each component part,
there is no ambiguity. But if the same part is offerend by more than
one vendor, we have to make a decision.

Vendors may have different cost models. Let's assume we want to buy a
gizmo and both vendor A and vendor B offer it. Vendor A can only
deliver 100 items, but is cheaper then vendor B who can supply 10,000.

We would then buy 100 gizmos from vendor A and the rest from vendor
B. However, this is a thing we cannot do if the analogy shall hold
(see \emph{Classification}). We have to choose a vendor for
gizmos and this decision needs to hold for some time. In that case we
would reason as follows:

\begin{itemize}
\item If we need 100 items or less, then we would buy them from the cheaper vendor A
\item If we need more than 100 items, then we have to buy them from vendor B
\end{itemize}

This has quite a number of consequences:
\begin{itemize}
\item It is not sufficient to know what we want to produce, we also
  need to know how much we want to produce.
\item It is not sufficient to know what each vender has to offer (and
  the price thereof), we also need to know how much they can sell, and
  probably more.
\end{itemize}

Now the number of items we want to produce may not have an upper
bound. When we continously produce items, the number of itmes and thus
the number of gizmos we need will increase as time goes by. Hence it
is difficult to answer the question ``how many gizmos do we need?''.

The number of gizmos show the same behaviour. Vendor A may only be
able to supply 100 gizmos now, but in the long he may supply a lot
more.

This means, that in order to construct a ``purchasing plan'' we need
to know for how long we want to leave in unchanged.





\end{document}
