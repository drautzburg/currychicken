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

\section {Cost models}
\subsection{Classification}

Sortplans typically don't operate on individual items. This is because
we want Sortplans to be valid for at least a number of days. However,
mail-items change from day to day and a Sortplan will see a given
mail-item typically only once.

Therefor we \emph{classify} mail-items into a number of classes, such
that the identity of a mail-item no longer plays a role and that more
than one mail-item can fall into the same class. In the example above
\emph{All parcels destined to your address} (``your parcels'') form
such a class. This definition is valid for quite some time and during
that time many parcels may fall into this class.

Likwise there is a classification for your ``neighbour's parcels''. If
we now define what the two processes \emph{you} and \emph{neighbour}
accept, we get the following table.

\begin{tabular}{l l l}
Process &class & cost\\
\hline
you & your parcels & low \\
you & neighbour's parcels & high \\
neighbour & your parcels & high \\
neighbour & neighbour's parcels & low \\
\hline
\end{tabular}

There is some ambiguity which needs to be resolved. However, when
you're not at home, then the \emph{you} process is not available and
this table is reduced to one without ambiguity.

\begin{tabular}{l l l}
Process &class & cost\\
\hline
neighbour & your parcels & high \\
neighbour & neighbour's parcels & low \\
\hline
\end{tabular}

\subsection{Adding value}

What if the postman decides to leave your parcel at the depot when
you're not at home but your neighbour \emph{is} at home. As far as
we've taken costs into account, this seems like an optimal solution,
because it avoids the high cost associated with delivering the parcel
to your neighbour. But obviously, in the real world this may not be
optimal at all.

There seems to be additional cost associated with not processing an
item at all. If this was not the case, a postal organisation could
simply not do anything at all as it is the best solution for
minimizing cost.

Another way of seeing this is that processing an item produces
value. Likewise, delivering to your neighbour may be seen as adding
less value, compared to delivering it directly to you. So value and
cost are essentially two sides of the same coin.

In the light of this reasoning, a postman is well advised to deliver a
parcel to your neighbour, if the added value outweighs the additional
cost, or if we associate delivering to your neighbout with less added
value instead of an additional cost, if the added value is still
positive.

There may well be situations, where this is not the case. If e.g. the
postman knows that you expect a parcel only the day after tomorrow,
then there is no point delivering it to your neighbour and it will be
better to try again in two days.

\subsection{Added value over time}
The added value created by delivering an item is what a postal
organization sells to its customers. Let's call it the \emph{delivery
  value} $dv$.

%\begin{figure}[htb!]
%\centering
%\includegraphics[width=4cm]{glass-slipper.jpg}
%\end{figure}

\end{document}
