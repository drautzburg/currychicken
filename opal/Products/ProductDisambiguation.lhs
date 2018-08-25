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
process. 

However, it may happen that for a given mailpiece there is more than
one possible downstream process. In that case a decision must be made,
which of the possible processes is \emph{best suited} for this
mailpiece.

This paper discusses this problem.
\end{abstract}


\tableofcontents 
\listoffigures

\section{Introduction}

Consider a distribution process with two outputs $O_1$ and $O_2$. The
task is to choose between the two outputs for any given mailpiece $m$,
such that the price we have to pay for the downstream ``disposal''
processes is minimized.

If we have a cost function associated with each output, then we can
for any given mailpiece simply calculate the cost for each output a
choose the cheaper one.

The situation is equivalent to purchasing an item from one of two
possible suppliers. We buy it from whoever offers the lowest price.

Disposing of items can be seen as purchasing a disposal service. Note
that we do not take care of the possibility that a downstream process
simply does to accept certain items. Instead we associate a
ridiculously high price with this situation.

While we eventually dispose or receive specfic items, the decision
itself shall not consider the items' identities. Instead items are
classified into $Products$, such that items which fall into the same
Product are treated the same.

In the purchasing example, this translates to the wish to by "a Gizmo"
and not a Gizmo with a specific serial number. In the disposal example
(which applies to mail processing), this translates to the need to
dispose a mailpiece with certain properties, while disregarding the
identity of the mailpiece.

While this seems pretty trivial, things become more complicated when

\begin{itemize}
\item the cost-function $C$ changes over time
\item the cost-function $C$ depends on the number of items to purchase
  or to dispose. Volume dependency may come as discounts or as limited
  availability of items or disposal capacity.
\item there is more than one consumer of items or services. You then
  get into a situation comparable to booking airline tickets, where
  the price for an additional ticket depends on other consumers.
\end{itemize}

\section{From trivial to general}

\subsection{The trivial case}

Suppose we have a classification function $P(m)$ which maps a
mailpiece to a Product. Furthermore assume that we have a cost
function $C_o(p)$ associated with each of the outputs |o|, which gives
us the price for accepting an item of product |p|.

Then we can trivially compute a distribution-function $O(p)$ which
gives us the optimal output for a given Product $p$. We can also
trivially compute our own cost function $C_y$ which tells us how much
we need to charge for accepting an item of Product $p$, assuming a
constant internal processing cost of $\Delta C$.

\begin{eqnarray}
  O(p) &&=
  \begin{cases}
    o_1 , & C_1(p) < C_2(p) \\
    o_2 , & otherwise
  \end{cases}\\
  C_y (p) &&= \Delta C + min (C_1(p), C_2(p))
\end{eqnarray}


\subsection{Cost changing over time}

Things are not so simple anymore, if the costs $C_i(p)$ change over
time, i.e. we have $C_i (t,p)$. We must then look for the lowest total
cost to process a given load.

Assume we have a given load $L(t,p)$ such that the number of items of
a certain product we have to process between $t_o$ and $t_1$ is

\begin{eqnarray}
  L_{tot}(p)     &&= L(t_1,p) - L(t_0,p)
\end{eqnarray}

Then the total cost to process a given product is


\end{document}
