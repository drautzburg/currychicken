\documentclass{article}

%include lhs2TeX.fmt
%include colorcode.fmt
%include greek.fmt
\renewcommand{\hscodestyle}{\small}
\definecolor{codecolor}{rgb}{0.95,0.95,1}
\colorhs
\usepackage{graphicx}
\usepackage{amssymb,amsmath}
\usepackage{cancel}
\usepackage{enumitem}
\usepackage{comment}
\usepackage{fancyhdr}
\pagestyle{headings}
\usepackage{needspace}
\usepackage{graphicx}
\usepackage{capt-of}
\usepackage{ragged2e}
\usepackage[parfill]{parskip}
\usepackage{ctable}
\usepackage{color}
\usepackage{colortbl}	
\usepackage{eurosym}
\usepackage{tikz}
\usetikzlibrary{calc} 
\usetikzlibrary{decorations}
\usetikzlibrary{plotmarks}
\usetikzlibrary{arrows}
\usetikzlibrary{chains,fit,shapes}
\usepackage{hyperref}
\hypersetup{colorlinks=true, linkcolor=blue, pdftoolbar=true}
\author{Martin Drautzburg}

\title{The Cinderella Algorithms}
\begin{document} \maketitle 

\begin{abstract}

        With the shrinking volume of lettermail, postal organizations
        find themselves more frequently in the situation where a
        mailman has to walk up an address only to deliver a
        \emph{single letter}. 

        Such single-letter deliveries are very costly. Postal
        organizations have therefore found ways to reduce the
        probablility for a single-letter devivery.

        This paper discusses a class of algorithms, called the
        Cinderella algorithms, which are an extension to existing
        approaches, and which reduce the probablility for
        single-letter-deliveries even further.

\end{abstract}

\begin{figure}[htb!]
\centering
\includegraphics[width=4cm]{glass-slipper.jpg}
\end{figure}

\tableofcontents 
\listoffigures

\section{Introduction}

\subsection{How to read this document}

This document first discusses algorithms on mathematical grounds and
then goes into simulating them. 

If the mathematical findings and the simulation results confirm each
other, we gain trust in both of them. The programming language we use
here is called \emph{Haskell}. The sourcecode is included in this
document.

\begin{code}
{-# LANGUAGE BangPatterns #-}
module SingleDelivery where
import Mail
import Plot
import Data.List
import Data.List.Utils
import Text.Printf
\end{code}

\needspace{20pt}
\subsection{Symbols
}
Throughout this document we will use the following symbols:

\begin{description}[style=multiline,leftmargin=1cm,font=\normalfont]
\item[$\lambda$] is the average number of letters per day and address
\item[$A$] is the total number of existing addresses.
\item[$L$] is the total number of Letters per day. Obviously $L = A
           \cdot \lambda$
\item[$P(n)$] is the probability for an address to receive $n$
              letters. 
\item[$Q(n)$] is the probability for a delivered-to address to receive
              $n$ letters. 
\item[$\delta$] is the Density i.e. the average number of letters per
                delivered-to address. 
\item[D] is the Desnisty in a log scale. The pseudo-unit of D is $mD$
\item[$C$]       stands for Cost
\item[$S$]       the \emph{scale} of the problem, which can be
                 computed from the number of addresses and other
                 values. Cost and cost savings are proportional to the
                 scale.
\end{description}


\subsection{Problem statement}

Not every address receives a letter every day. A typical value is a
total of one letter every week. Then the number of letters a given
address receives on an average day is

\begin{align}
        \lambda   = \frac{1}{6} 
\end{align}

Single-letter deliveries are costly. The goal of this paper is to find
and evaluate algorithms which reduce this cost, at the price of
delaying letters.

A prominent algorithm is the \emph{vanilla} approach, a non-algorithm,
which simply gets today's mail out of the door. The more sophisticated
algorithms all depend on the presence of a \emph{store}.


\section{Probability estimations and Metrics}
There are several ways to evaluate algorithms, which attempt to reduce
single-letter deliveries

\begin{description}
\item [$P$ - Single-letter addresses] The proportion of addresses receiving
  a single letter to the total number of addresses. Something like
  15\% of all addresses may receive a single letter. As it turns out,
  this metric is not very useful for evaluating algorithms.

\item [$Q$ - Single-letter deliveries] The proportion of addresses receiving
  a single letter to the number of addresses receiving mail. This
  number is usually much larger, because addresses receiving no mail
  do longer lower the result. Something like 90\% of all deliveries
  carry only a single letter. This is a better metric for evaluating
  algorithms.

\item[D or $\delta$ - Density] The average number of letters per
  delivered-to address $\delta$ or the corresponding log value $D$. D
  is 0 or larger, $\delta$ is 1 or larger, the larger the better. This
  is the best metric for evaluating algorithms, because the annual
  savings can directly be computed from the Density.

\end{description}

\section{Mathematical fundamentals}


Some interesting probabilitites can be computed from the Poisson
distribution, when we assume an even distribution of letters over
addresses\footnote{We do however, run simulations with different
  distributions later.}.
%
\begin{align}
\text{(Poisson:)\quad} P_{\lambda}(k)     &= \frac{\lambda^k}{k!} e^{-\lambda} \label{poisson}\\
\bigskip\notag\\
\mathbf{\Rightarrow P_{\lambda}(0)}       &= \mathbf{e^{-\lambda}\label{q0} }\\
\mathbf{\Rightarrow P_{\lambda}(1)}       &= \mathbf{\lambda \cdot e^{-\lambda}} \\
\mathbf{\Rightarrow P_{\lambda}(\bar{0})} &= \mathbf{1 - e^{-\lambda} \label{d}}
\end{align}

These are the probablilites for an address to receive zero, exactly
one or any number of letters greater than zero.

\section{The Vanilla approach}
\subsection{Estimating single-letter addresses $P$}

The probablity $P$ for an address to receive a single letter is
$P_{\lambda}(1)$. With \hbox{$\lambda=\frac 1 6$}
%
\begin{align}
 \Rightarrow P_{\frac 1 6}(1) = 0.141 = 14.1 \%
\end{align}

\begin{figure}[htb!]
\centering
\begin{tikzpicture}[domain=0.05:2, scale=4]
\draw[very thin,color=gray, step=0.25cm] (0,0) grid (2,0.5);
\draw[thick, color=red] plot[smooth] (\x,{\x*exp(-\x)}) node[right, yshift=-3pt]{$P_{\lambda}(1)$};
\foreach \x in {0,0.5, 1, 1.5}
    \draw (\x,0) -- (\x,-0.02) node[below] {$\x$};
\foreach \y in {0, 0.25, 0.5}
    \draw (0,\y) -- (-0.05,\y) node[left] {\y};

\path node at (2.2,-0.1) {$\lambda$};

\end{tikzpicture}
\caption{Calculated single-letter addresses vs. $\lambda$}
\label{fig:qvsl}
\end{figure}

Interestingly adding more letters, i.e. increasing $\lambda$ results
in \emph{more} single-letter addresses, as long as $\lambda<1$.

\subsection{Estimating single-letter deliveries $Q$}

A more interesting metric is the ratio of single-delivery addresses to
addresses which receive mail. The above estimation had the
\emph{total} number of addresses in the denominator, including those
which do not receive mail at all on a particular day.

The probability for an address to receive a single letter under the
condition that it receives 1 or more letters (i.e. a nonzero number of
letters) is a matter of conditional probability, which can be written
as

\begin{align}
P (1 \mid \bar{0}) = \frac{P_{\lambda}(1) \cap \cancel{P_{\lambda}(\bar{0})}}{P_{\lambda}(\bar{0})}
\end{align}

The $P_{\lambda}(\bar{0})$ in the numerator does not change anything, because
every letter which receives exactly one letter also receives nonzero letters.

Thus
\begin{align}
Q_{\lambda}(1) = P (1 \mid \bar{0}) = \frac{\lambda \cdot e^{-\lambda}}{1 -  e^{-\lambda}}
\end{align}


For $\lambda=\frac 1 6 \Rightarrow Q_{\frac 1 6}(1) =0.9189 = 91.89\%$.

So in roughly 9 out of 10 cases the mailman hat to walk up an address
only to deliver a single letter. 

Interestingly a larger $\lambda < 1$ lowers the single-letter
\emph{deliveries} while increasing the single-letter
\emph{addresses}. This means that - with growing $\lambda$ - more
addresses receive only a single letter, but even more addresses
receive more than one letter. In other words: $P_{\lambda}(\bar{0})$
grows faster than $P_{\lambda}(1)$

%
\begin{figure}[htb!]
\centering
\begin{tikzpicture}[domain=0.05:0.5, scale=8, x=2cm]
\draw[very thin,color=gray, step=0.1cm] (0,0.499) grid (0.5,1);
\draw[thick, color=red] plot[smooth] (\x,{\x*exp(-\x)/(1-exp(-\x))}) node[right, yshift=-3pt]{$Q_{\lambda}(1)$};
\draw [very thick, color=blue](0.166, 0.92) circle (0.2pt) node [below] {$\lambda = \frac 1 6$};
\foreach \x in {0.1, 0.2,0.3, 0.4, 0.5}
    \draw (\x,0.5) -- (\x,0.49) node[below] {$\x$};
\foreach \y in {0.5, 0.6, 0.7, 0.8, 0.9, 1.0}
    \draw (0,\y) -- (-0.01,\y) node[left] {\y};

\end{tikzpicture}
\caption{Calculated single-letter deliveries vs. $\lambda$}
\end{figure}

\subsection{Estimating density of letters}

To compute $\delta$, the average number of letters per address, which
receives at least one letter, we recall that the total number of
letters $L$ is $A\cdot\lambda$
%
\begin{align}
\delta_{\lambda}                          &= \frac{L}{A_{\bar{0}}}  \notag\\
(\text{with (\ref{d})}\Rightarrow)   &= \frac {A \cdot \lambda} {A \cdot(1 - e^{-\lambda})} \notag\\
\Rightarrow \delta_{\lambda}              &= \frac{\lambda}{1 - e^{-\lambda}} \label{density}
\end{align}

\begin{figure}[htb!]
\centering
\begin{tikzpicture}[domain=0.05:0.5, scale=8, x=2cm]
\draw[very thin,color=gray, step=0.1cm] (0,1.0) grid (0.5,1.4);
\draw[thick, color=red] plot[smooth] (\x,{\x/(1-exp(-\x))}) node[right, yshift=-3pt]{$\delta_{\lambda}$};
\draw [very thick, color=blue](0.166, 1.0856) circle (0.2pt) node [below] {$\lambda = \frac 1 6$};
\foreach \x in {0.1, 0.2,0.3, 0.4, 0.5}
    \draw (\x,1.0) -- (\x,0.99) node[below] {$\x$};
\foreach \y in {1.0, 1.1, 1.2, 1.3, 1.4}
    \draw (0,\y) -- (-0.01,\y) node[left] {\y};

\end{tikzpicture}
\caption{Calculated Density vs. $\lambda$}
\end{figure}

\begin{align}
\text{with\quad}\lambda  &= \frac{1}{6} \notag\\
\delta_{1/6}              &= 1.0856
\end{align}

So with $\lambda$ being so low, the mailman will not be able to
deliver much more than one letter per delivered-to address on average.

In practice we will always encounter densitites, which are just
slightly above 1. Those numbers are hard to distinguish. We therefore
introduce a logarithmic value $D$ with the unit $mD$ (think:
decibel)\footnote{the
  $m$ stands for \emph{milli} and the $D$ for \emph{Density}. "mD" is
  ponounced "milli-dees", but feel free to read it as \emph{martin
  Drautzburg}}.

\begin{align}
D                  &= 1000 \cdot log(\delta) \\
\delta             &= 10^{\frac D{1000}} \\
D_{1/6}           &= 35.66 \quad mD
\end{align}

\begin{code}
-- function to covert $\delta$ to mD
inMilliDee dens = 1000 * logBase 10 dens
\end{code}


\subsection{Cost Estimation and potential savings}

With the total number of letters being $L = A\cdot\lambda$ and a
Density of $\delta$ we have to deliver to $A_{\bar{0}} =
\frac{A\cdot\lambda}{\delta}$ addresses. Assuming a cost of $C_A$ per
address and a (smaller) cost of $C_L$ per letter, the total cost is.
%
\begin{align}
C_D(\delta) &= \frac{A\cdot\lambda}{\delta}\cdot C_A + A\cdot\lambda \cdot C_L \notag\\
     &= A\cdot\lambda \left(\frac{C_A}{\delta} + C_L\right)
\end{align}

To compute the cost \emph{savings} we must subtract the cost from one
algorithm from the cost of a reference algorithm. This leads to
%
\begin{align}
\Delta C_D(\delta) &= A\cdot\lambda \left(\frac{C_A}{\delta_0} +
       \cancel{C_L}\right) - A\cdot\lambda \left(\frac{C_A}{\delta} +
       \cancel{C_L}\right) \notag\\
                &= A\cdot\lambda \cdot C_A \left(\frac{1}{\delta_0} -
                \frac{1}{\delta} \right) \label{saving} \\
                &= S \cdot \left(\frac{1}{\delta_0} -
                \frac{1}{\delta} \right)
\end{align}
%

The cost-per-letter $C_L$ gets cancelled out, because no matter how
cleverly we organize the delivery, i.e. no matter how large we make the
Density, we will still have to deliver the same number of letters.

\subsubsection{Discussion}

The factor $S_D = A\cdot\lambda \cdot C_A$ is a scaling factor, which
expresses, that the potential savings increase with the number of
letters, the number of addresses and the cost per delivery. It is more
related to the size of the problem than to the quality of the
solution.

Some reasonable values for a small country could look like this:
%
\begin{align}
\lambda                         &= \frac 1 6 \notag\\
A                               &= 6\cdot 10^6 \notag\\
(\Rightarrow L                  &=  1\cdot 10^6)\notag\\
C_A                             &= 0.2 \text{\euro}\notag\\
S_D = A\cdot\lambda \cdot C_A       &= 200000\text{\euro}
\end{align}

The value $A\cdot\lambda$ is the number of letters \emph{per day}. To
compute the \emph{annual} savings, we must multipy by the number of
delivery days, which we assume to be 200.

\begin{align}
S_Y        &= days\cdot A \cdot \lambda \cdot C_A  = 40 \cdot 10^6 \text{\euro}\\
\Delta C_Y &= \left(\frac{1}{\delta_0} - \frac{1}{\delta} \right)
\cdot S \\
\Delta C_Y &= \left(\frac{1}{\delta_0} - \frac{1}{\delta} \right)
         \cdot 40 \cdot 10^6 \text{\euro} 
\end{align}

\begin{figure}[htb!]
\centering
\begin{tikzpicture}[domain=1.08:1.2, scale=0.8, x=40cm]
\draw[very thin,color=gray, step=1cm] (0.999,0) grid (1.2,5);

\draw[thick, color=red] plot[smooth] (\x,{40/1.0856 - 40/\x}) node[right, yshift=-3pt]{$\Delta C_Y$};

\draw [very thick, color=blue](1.0856, 0) circle (2pt) node [above, xshift=-12pt] {vanilla};
\foreach \x in {1.0, 1.05, 1.1, 1.15, 1.2}
    \draw (\x,0) -- (\x,-0.2) node[below] {$\x$};
\foreach \y in {0, 1,2,3,4,5}
    \draw (1,\y) -- (0.995,\y) node[left] {\y};
\path node at (1.22,-0.4) {$\delta$};
\end{tikzpicture}
\caption{Calculated annual savings vs. $\delta$ in $10^6$\euro}
\end{figure}

Because $\delta_0$ and $\delta$ will in practice both be very close to
one and $\frac{1}{\delta_0} - \frac{1}{\delta}$ will be very close to
zero, the scaling factor $S$ needs to be investigated first to decide
whether an alternative algorithm is promising or not.

With $\lambda = \frac 1 6$ it is very difficult to increase the
density to values beyond $\delta> 1.1$ or $D > 45$.

Therefore $S = 40 \cdot
10^6 \text{\euro}$ is too small to justify an investment in new
algorithms. This applies to xy-Soring too: with $S$ as small as that,
xy-Sorting would have been a waste. 


\begin{figure}[htb!]
\centering
\begin{tikzpicture}[domain=35.0:55.0, scale=0.3, y=2cm]

\draw[very thin,color=gray, step=2cm, xstep=2.5cm] (35,0) grid (55,5);

\draw[thick, color=red] plot[smooth] (\x,{40/1.0856 - 40/10^(\x/1000)}) 
             node[right,yshift=-3pt]{$\Delta C_Y$};

\draw [very thick, color=blue](35.66, 0) circle (4pt) 
node [above,xshift=-12pt] {vanilla};

\foreach \x in {35, 40, 45, 50, 55}
    \draw (\x,0) -- (\x,-0.2) node[below] {$\x$};

\foreach \y in {0, 1,2,3,4,5}
    \draw (35,\y) -- (34.5,\y) node[left] {\y};
\path node at (57,-0.4) {$D$};
\end{tikzpicture}
\caption{Calculated annual savings vs. $D$ in $10^6$\euro}
\end{figure}


$S$ is very sensitive to the $A, \lambda$ and $C_A$. If the cost per
delivery $C_A$ is not 0.2\euro{} but 0.3\euro, the potential savings
increase by 50\%

\subsection{Estimating letters per delivered address with store}

If we are allowed to keep main in store, we can reduce single-letter
deliveries and improve the density. However, I was not able to
mathematically compute the effect of the store precisely. The
following reasoning is only an approximation.

After $d$ days we can only get a single-letter address if a letter was
first seen $d$ days ago and there was no other letter for the same
addresse ever since. 

The probalitity for receiving no letter at all was shown in (\ref{q0})
%
\begin{align}
\text{with\quad}\lambda     &= \frac{1}{6} \notag\\
P_{1/6}(0)                  &= 0.8465 = 84.65\%
\end{align}

In a best-case scenario, i.e. if we have no obligations other than
delivering after $d$ days, each day reduces the percentage of
single-delivery addresses by a factor of $e^{-\lambda}$ or $0.8465$
for $\lambda=\frac 1 6$. More generally:
%
\begin{align}
P_{d\lambda}                 &= \lambda\cdot e^{-\lambda} \cdot \left(e^{-\lambda} \right)^d\\
                             &= \lambda\cdot e^{-\lambda(d+1)}
\end{align}

\begin{figure}[htb!]
\centering
\begin{tikzpicture}[domain=0:5, scale=1.5, y=10cm]
\draw[very thin,color=gray] (0,0) grid (5,0.3);
\draw[thick, color=red] plot[smooth] (\x,{1/2*exp(-1/2*(\x+1))}) node[right, yshift=-3pt]{$P_{\lambda=\frac 1 2}$};
\draw[thick, color=blue] plot[smooth] (\x,{1/6*exp(-1/6*(\x+1))}) node[right,yshift=3pt]{$P_{\lambda=\frac 1 6}$};
\foreach \x in {0,1,2,3,4,5}
    \draw (\x,0) -- (\x,-0.02) node[below] {$\x$};
\foreach \y in {0.1, 0.2, 0.3}
    \draw (0,\y) -- (-0.1,\y) node[left] {$\y$};

\path node at (5.4,-0.02) {$days$};

\end{tikzpicture}

\caption{{Calcualted effect of keeping mail in store}}
\end{figure}

This is only an approximation, because each day we loose some letters,
i.e. those letters which can be merged with letters from $d$ days ago,
without causing a single-letter delivery. So we will have fewer
letters to begin with.

For this reason we will not do any further calculation on the effect
of the store.

\section{XY Sorting}
\subsection{Algorithm}
A traditional approach to reduce single-letter deliveries is called XY
sorting, where low-priority mail (Class-B mail) is only delivered
every other day.

\begin{figure}[htb!]
\centering
\centering

\begin{tikzpicture}[node distance=0.8cm]
\tikzstyle{box}=[draw, fill=blue!20, minimum size=2em, minimum width=3.5cm, rounded corners]
\tikzstyle{comment}=[draw, fill=lightgray!20, rounded corners]
\draw [<->](-1.75,1) -- (5.25,1) ;
\node at(1.5,0.7) {mail leaving today};

\draw [<->](5.75,1) -- (9.25,1) ;
\node at(7.5,0.7) {to store};

\node (A) at (0,0) [box] {Class-A, X};
\node (B) at (3.5,0)[box] {Class-A, Y};
\node (C) at (0,-1)[box] {Class-B, X};
\node (D) at (7.5,-1) [box, fill=lightgray] {Class-B, Y};
\node (E) at (0,-2) [box, fill=lightgray] {Class-B, X};
\node (e1) at (3,-2) [comment]{$\leftarrow$from store};

\end{tikzpicture}
\caption{Schematic of XY-sorting}
\end{figure}


We assume, the proportion of Class-A to Class-B mail is roughly
\begin{align}
        A  &: 75\% \notag\\
        B  &: 25\% \notag
\end{align}

We will split the set of addresses into two equqally sized subsets,
called $x$ and $y$. 

\needspace{60pt}
On odd days we deliver 
\begin{itemize}
\item all of today's Class-A mail
\item the Class-B mail for $x$-addresses from today 
\item the Class-B mail for $x$-addresses from yesterday 
\end{itemize}

On even days we deliver 
\begin{itemize}
\item all of today's Class-A mail
\item the Class-B mail for $y$-addresses from today 
\item the Class-B mail for $y$-addresses from yesterday 
\end{itemize}

Thus each addresses receives Class-B letters only every other day and
the Class-B mail for any address is accumulated over two days.

\subsection{Estimating single-letter addresses}

To compute the probablities we consider an odd day. For even days the
situation is essentially the same.

\begin{description}

\item[x-addresses] will receive the usual $\frac{1}{6}$ letters per
  address. However they also receive yesterdays's Class-B mail, which
  increases the number of letters by 25\%. So in total $\lambda_x =
  1.25 \cdot \frac{1}{6}$

\item[y-addresses] only receive Class-A mail, which is 75\% of all
  mail. So $\lambda_y = 0.75 \cdot \frac{1}{6}$

\end{description}

The probabilies to receive a single letter then become:
\begin{align}
P_{\lambda x}(1)         &= 16.92\% \label{xpct}\\
P_{\lambda y}(1)         &= 11.03\% \label{ypct} \\
P_{avg}                  &= 13.97%
\end{align}

The fact that we get \emph{more} single-letter addresses on an $x$ day
is owed to equation (\ref{q0}), and it plot, shown in Figure
\ref{fig:qvsl}. For low $\lambda$s, more letters lead to more
single-letter addresses. Bear in mind that $P$ does not say much about
potential savings. Still on average there is an improvement.


\subsection{Estimating single-letter deliveries}

To estimate $Q$, the number of single-letter deliveries, we divide the
number of single-letter addresses by the number of addresses which
receive mail. We do this both for the $x$ and the $y$ part, where the
$x$ part has a $\lambda$ which is increased by a factor of $1+b$,
while the $y$'s $\lambda$ is reduced to $\lambda\cdot(1-b)$

This leads to the following formula
\begin{align}
  Q_{xy}(1)  &= \frac{\frac A 2 \lambda(1+b) e^{-\lambda(1+b)} + \frac A 2 \lambda(1-b) e^{-\lambda(1-b)} }
  {\frac A 2 (1-e^{-\lambda(1+b)}) + \frac A 2 (1-e^{-\lambda(1-b)})} \notag\\
  \notag\\
  Q_{xy}(1)  &= \lambda\frac{(1+b)e^{-\lambda(1+b)} + (1-b)e^{-\lambda(1-b)}}{2 - e^{-\lambda(1+b)} - e^{-\lambda(1-b)}}
\end{align}

With $\lambda=\frac 1 6$ and $b=0.25$ we get
\begin{align}
  Q_{xy}(1)  &= 0.91458 = 91.458\%
\end{align}

\subsection{Estimating Density}
In a similar fashion, and using equation (\ref{density}) we end up with

\begin{align}
\delta_{xy} &= \frac{\lambda(1+b) + \lambda(1-b)}{1 - e^{-\lambda(1+b)} + 1 - e^{-\lambda(1+b)}} \notag\\
  \notag\\
\delta_{xy} &= \frac{2\lambda}{2 - e^{-\lambda(1+b)} - e^{-\lambda(1-b)}}
\end{align}

With $\lambda=\frac 1 6$ and $b=0.25$ we get
%
\begin{align}
  \delta_{xy}  &= 1.09086 \\
  D_{xy}       &= 37.76
\end{align}

\begin{code}
savingsFromD d = scale * (1.0/1.0856 - 1.0/d)
        where
            scale = 200.0 * 6e6 * (1.0/6) * 0.2


savingsFromMD d = savingsFromD (10**(d/1000))

savingsFromMD' md = scale * (log 10) / 1000 * (md - 35.66)
        where
            scale = 200.0 * 6e6 * (1.0/6) * 0.2

\end{code}


\section{Simulation framework}
\subsection{The general approach}
XY is a special case of an algorithm where a certain part of the Mail
does not leave the Sorting Center. This part is kept in store and gets
processed in one of the following days
.
\tikzstyle{box}=[draw, fill=blue!20, minimum size=2em, minimum width=2cm, rounded corners]
\tikzstyle{inp} = [pin edge={to-,thin,black}]
\tikzstyle{outp} = [pin edge={-to,thin,black}]
\begin{figure}[htb!]
\centering
\begin{tikzpicture}[node distance=3.5cm,auto,>=latex']
    \node [box, 
          pin={[inp]above:$in_1$},
          pin={[outp]below:$out_1$}
          ] (a) {$day_1$};
    \node (b) [left of=a,node distance=2cm, coordinate] {a};
    \node [box, 
          pin={[inp]above:$in_2$},
          pin={[outp]below:$out_2$}
          ] (c) [right of=a] {$day_2$};
    \node (d) [left of=a,node distance=2cm, coordinate] {c};
    \node [box, 
          pin={[inp]above:$in_3$},
          pin={[outp]below:$out_3$}
          ] (e) [right of=c] {$day_3$};
    \node [coordinate] (end) [right of=e, node distance=3cm]{};
    \path[->] (a) edge node {$store_1$} (c);
    \path[->] (c) edge node {$store_2$} (e);
    \draw[->] (e) edge node {$store_3$} (end) ;
\end{tikzpicture}
\caption{Schematic of the general approach}
\end{figure}


\subsection{Simulation}

To run a simulation we assume a function which simulates the
processing on a single day. This function is the \emph{Testee},
i.e. the algorithm under test. It takes the incoming $in_i$ mail and
the $store_{i-1}$ mail and computes the outgoing $out_i$ mail and the
new $store_i$ mail.

Applying ("folding") this function over a number of $n$ days would
give us the $store_n$ of the last day. However, we are also interested
in collecting some statistics from earlier days. Therefore we need a
\emph{runnig state} which does not only contain the store but also
additional "logging" information.



\subsubsection{The running State}

Let's first define the running state There shall be a global state,
which remains constant throughout the simulation. Then there shall be
a \verb+DayLog+ which contains results for each day and the
\verb+Store+ which contains the Mailpieces in store\footnote{The
  handling of \verb+State+ may appear overcomplicated, but remember,
  that Haskell is a pure functional language, which does not know
  mutable state. Still, just 5 pages of fully documented code it
  pretty terse for a simulation}.

\begin{code}
type State = (Globals, Store, [DayLog])
type Store = Mail

-- extract the daylogs from the state 
dayLogs :: State -> [DayLog]
dayLogs (_,_,dls) = dls
\end{code}

\verb+Globals+ and the \verb+DayLog+ consist of records:

\begin{code}
data Globals = Globals {
    totalAddresses :: Int,
    lambda         :: Double
}deriving (Ord, Eq,Show)
\end{code}

\needspace{40pt}
\begin{code}
data DayLog   = DayLog {
            day       :: Int,
            mailIn    :: Int,
            mailOut   :: Int,
            singles   :: Int,
            singlesP  :: Double,
            singlesQ  :: Double,
            density   :: Double,
            store     :: Int,
            comment   :: String
        } 

-- convert a \verb+DayLog+ to string

instance Show DayLog where
        show dl   = showInt   "day"       day
                  ++ showInt  "mailIn"    mailIn
                  ++ showInt  "mailOut"   mailOut
                  ++ showInt  "store"     store
                  ++ showInt  "singles"   singles
                  ++ showPct  "singlesP"  singlesP
                  ++ showPct  "singlesQ"  singlesQ
                  ++ showDbl  "density"   density
                  ++ showStr  "comment"   comment
                where
                    showInt name f   = name ++ "=" ++ (printf "%-5d, " (f dl))
                    showDbl name f   = name ++ "=" ++ (printf "%-.5f, " (f dl))
                    showPct name f   = name ++ "=" ++ (printf "%.2f%%, " (100 * f dl))
                    showStr name f   = name ++ "=" ++ f dl
                 

\end{code}


To print the \verb+Globals+ and the \verb+DayLog+s we use the following
function:

\begin{code}
printLogs :: State -> IO()
printLogs (g,s,d) = do
    putStrLn ""
    putStrLn ("Average density: " ++ (show avg))
    putStrLn ("Average density: " 
              ++ (printf "%+-.2f" $ inMilliDee avg) ++ " mD")
    putStrLn ("Total mailOut  : " ++ (show totalMail))
    putStrLn ""

    printLogs' (g,s,d)
        where
            totalDensity = sum $ zipWith (*)  densities mailOuts
            densities = map density d
            mailOuts = map (fromIntegral . mailOut) d
            totalMail = sum (map mailOut d)
            avg = totalDensity / (fromIntegral totalMail)


printLogs' (globals, _, [])   = do
    putStrLn $ show globals
    return ()
printLogs' (globals, s, dl:dls)   = do
            printLogs' (globals, s, dls)
            putStrLn $ show dl
            return ()
\end{code}

For the plotting functions in the appendix, we need to extract values
for each day.

\begin{code}
extract :: (Num a) => (DayLog -> a) -> State -> [a]
extract accessor state   = map accessor (dayLogs state)
\end{code}



\subsubsection{The main simulation}

The \emph{Testee}, i.e. the algorithm under test has the following type:

\begin{code}
type SimDay   = Mail -> State -> State
\end{code}

The inputs to a simulation are 
\begin{itemize}
\item the random seed \verb+Seed+
\item the number of days to simulate \verb+Ndays+
\item the number of mailpieces each day \verb+Mvpd+
\item the \verb+Alpha+ controlling the skew 
\item the letters per address and day \verb+lambda+
\item the \verb+SimDay+ function (the testee)
\end{itemize}


\begin{code}
type Simulate  = Seed -> Ndays -> Mvpd -> Alpha -> Lambda -> SimDay -> State
type Seed      = Int
type Ndays     = Int
type Mvpd      = Int
type Alpha     = Double
type Lambda    = Double
\end{code}

The actual iteration is a simple \verb+foldr+. What remains to be done
is generating an initial state and some random mail
%
\begin{code}
simulate' :: Simulate
simulate'  seed nDays mvpd alpha lambda simDay  = foldl' simDay'  initState mailIn
        where
          --------------------------------------------
          -- trying to solve space problems
          foldl' f !z []     = z
          foldl' f !z (!x:xs) = let z' = z `f` x 
                              in seq z' $ foldl' f z' xs
          simDay' !s !m = simDay m s
          --------------------------------------------
          -- generate some random mail (see the \verb+Mail+ package)
          !mailIn  = take nDays $ mail seed alpha lambda mvpd 0

          -- Initialize the global state
          globalState  = Globals {
                             lambda          = lambda,
                             totalAddresses  = fromIntegral $ scMax lambda mvpd
                         }
          initState    = (globalState, emptyMail, [])
\end{code}

We can freeze a few parameters and only leave the testee variable.
%
\begin{code}
simulate simDay = simulate'  1234 14 20000 1 (1/6) simDay
\end{code}

Since we will always collect the same information in the
\verb+DayLog+s, and we always use the same definition of \verb+Mail+,
we can write a function which populates the \verb+DayLog+:

\begin{code}
newDayLog :: State -> MailIn -> MailOut -> Store -> DayLog
type MailIn    = Mail
type MailOut   = Mail
newDayLog state@(g,s,d) mailIn mailOut toStore = 
        DayLog {
         day        =  fromIntegral $ length d,
         mailIn     =  countMpcs mailIn,
         mailOut    =  countMpcs mailOut,
         store      =  countMpcs toStore,
         singles    =  countSingles mailOut,
         singlesP   =  (countSingles mailOut)/(fromIntegral $ totalAddresses g),
         singlesQ   =  countSinglesQ mailOut,
         density    =  countDensity mailOut,
         comment    = ""
        }
\end{code}


\section{Simulating existing approaches}

\subsection{Vanilla approach}
\begin{code}
vanilla :: SimDay
vanilla mail state@(g, s, d) = (g, toStore, logEntry:d)
        where
            logEntry   = newDayLog state mail mail toStore
            toStore    = emptyMail
\end{code}

To plot the single-letter deliveries we use:

\begin{code}
plotVanillaQ = plotQ 
               "plotVanillaQ.tex" 
               "Q(1)-plot of the vanilla algorithm" 
               (100.0 * 0.9189) -- calculated value
               values
                       where
                           values = extract singlesQ (simulate vanilla)
\end{code}

\input{plotVanillaQ}

\subsection{XY sorting}

For XY sorting we always process mail in store together with today's
mail. We send half of the Class-B letters to store.

To distinguish $x$ and $y$ addresses we use the following:

\begin{code}
data XY = X | Y deriving (Eq, Show)
addr :: XY -> Sortcode -> Bool 
addr X sc   = (sc `mod`  2) == 0
addr Y sc   = not $ addr X sc
\end{code}

Furthermore we need to distinguish between odd and even days.

\begin{code}
today :: State -> Int
today (_, _, [])       = 0
today (_, _, dl:dls)   = 1 + day dl

 
\end{code}

\begin{code}
xySorting :: SimDay
xySorting mailIn state@(g, s, d) = (g, toStore, logEntry:d)
        where
            now = today state
            xy | now `mod` 2 == 0  = Y
               | otherwise         = X
            allMail = mergeMail [mailIn, s]
            (mailOut, toStore)    = splitMailBy (outToday xy) allMail

            outToday :: XY -> Mpc -> Bool
            outToday _ (Mpc 'A' _ _)    = True
            outToday a (Mpc 'B'  sc _)  = addr a sc

            logEntry = (newDayLog state mailIn mailOut toStore) 
                       {comment = (show xy)}
\end{code}

To plot the single-letter deliveries we use:

\begin{code}
plotXyQ = plotQ 
          "plotXyQ.tex" 
          "Q(1)-plot of the xy-algorithm" 
          (100.0 * 0.91458) values
                  where
                      values = extract singlesQ (simulate xySorting)
\end{code}

\input{plotXyQ}

On the first day, we cannot pull mail from store, which results in an
exceptionally low $\lambda$ and a poor $Q$. On the following days, the
simulation is well in line with the calculation.

\section{The Cinderella Algorithm}
\subsection{Cinderella with one-day store}

If we knew ahead of time, which Class-A letters to expect, then things
would be fairly simple. We would simply reject all Class-B letters for
whose addresses there is no Class-A letter and save them for
tomorrow. We would have to make sure, Class-B letters don't get stored
for too long, because there is a service-level-agreement to be obeyed.

The above is a technique, which relies on pre-knowledge. However, we
have set our hearts to improving XY \emph{without} depending on
pre-knowledge.

The core of the idea is as follows: consider the very last letter for
which we have to decide whether it is good or bad. If we already
accepted another letter for the same address, then it will be a good
letter, because it will not cause a single-letter delivery. Otherwise
it will \emph{certainly} be a bad letter, because there is no other
letter to be expected, which could go to the same address.

For the letter before the last one, the situation is more fuzzy. Again
we will accept it as good, if we already accepted another letter to
the same address. However, if we didn't, then the letter can still be
good, as there is another, unseen letter which could go to the same
address. The chances for this to happen aren't overwhelming, because
it is not very likely that the single last letter will got to the same
address.

As we consider earlier and earlier letter we become less and less
certain whether or not we can rule out a letter.  In a way, we build
up some pre-knowlege as we separate good letters from bad
letters. Initially, when we decide about the first letter, our
pre-knowledge will be zilch, at the end, when we've seen all but one
letter our pre-knowledge will be perfect. So on \emph{average} we know
much more than nothing.

\begin{code}
cinderella1 :: SimDay
cinderella1 mailIn state@(g, s, d) = (g, toStore, logEntry:d)
        where
            (first, second)    = splitMailAt 0.5 mailIn

            -- unconditionally output the first half of todays mail and
            -- everything from store (so it does not age too much)
            out1               = mergeMail [first,s]

            -- from the second half, only output the class-A ones and
            -- the ones which have a sortcode
            -- which is already present in out1. The others go to store

            (out2, toStore)    = splitMailBy (
                                              \mpc -> (mc mpc == 'A') 
                                                      ||(containsSc out1 mpc)) second

            mailOut            = mergeMail [out1,out2]
            logEntry           = (newDayLog state mailIn mailOut toStore) 

\end{code}

\subsection{Cinderella with multi-day store}

Things get even better, when we can store mail for more than just a
single day. Still the store will eventually contain mail, which is so
old, that it \emph{must} be delivered today. If we additionally
deliver younger mail from store, which happens to match one of the old
items, then we get a pretty good density.

Knowing what old items you have in store is easy if you have a
Cinderella-like information system at hand. However, separating mail
which matches old mail is not so easy.

You \emph{can} run the entire store through the main sorting run and
do this separation on-the-fly. However, with mail from several days in
store you add considerable load to the main sorting run. The main
sorting run typically has tight time constraints, because other
sorting runs and transports are waiting for its output. The situation
gets worse, when the storage time increases.

\begin{figure}[htb!]
\centering
\begin{tikzpicture}

\tikzstyle{every path}=[very thick]
\tikzstyle{tmtape}=[draw,minimum size=0.5cm, fill=blue!20]
\tikzstyle{store}=[draw, fill=lightgray!20, minimum size=2em, minimum
width=3.5cm, rounded corners]

\node (Store) at (1,4) [store] {Store};

\begin{scope}[start chain=1 going right,node distance=-0.2mm]
 \node [on chain=1,tmtape] (A1) {A};
    \node [on chain=1,tmtape] {B};
    \node [on chain=1,tmtape] {C};
    \node [on chain=1,tmtape] {D};
    \node [on chain=1,tmtape] {$\ldots$};
    \node [on chain=1,tmtape, minimum width = 1.4cm,
    fill=lightgray!20] (Bad1) {bad};
\end{scope}


%% bad outputs

\draw[->] (Bad1.north).. controls +(up:1cm) and +(right:1cm) ..
(Store.east); 

\draw[->, color=blue] (Store.west).. controls +(left:1cm) and +(left:1cm) ..
(A1.west); 


\node (Today) at (-3,2) {Todays's mail} ;
\path[draw, ->, color=blue]  (Today) .. controls +(down:1cm) and +(left:1cm) ..
(A1.west) ;

\end{tikzpicture}
\caption{Schematic of Cinderella with multi-day store}
\end{figure}


Alternatively you can do the separation between good and bad mail in
store by means of an additional sorting run.  The majority of mail
from store is bad mail and will be fed back to the store. Then you
would only have to run the \emph{good} mail though the main sorting
run, which takes much less time.


\begin{figure}[htb!]
\centering
\begin{tikzpicture}

\tikzstyle{every path}=[very thick]
\tikzstyle{tmtape}=[draw,minimum size=0.7cm, fill=blue!20]
\tikzstyle{store}=[draw, fill=lightgray!20, minimum size=2em, minimum
width=3.5cm, rounded corners]

\node (Store) at (0,4) [store] {Store};

\begin{scope}[start chain=1 going right,node distance=-0.2mm]
 \node [on chain=1,tmtape] (A1) {A};
    \node [on chain=1,tmtape] {B};
    \node [on chain=1,tmtape] {C};
    \node [on chain=1,tmtape] {D};
    \node [on chain=1,tmtape] {$\ldots$};
    \node [on chain=1,tmtape, minimum width = 1.4cm,
    fill=lightgray!20] (Bad1) {bad};
\end{scope}

\begin{scope}[shift={(0cm,2cm)},start chain=1 going right,node distance=-0.2mm]
 \node [on chain=1,tmtape, minimum width = 1.4cm,fill=blue!20] (Good1) {good};
    \node [on chain=1,tmtape, minimum width = 4cm,
    fill=lightgray!20] (Bad1) {bad};
\end{scope}


%% bad outputs

\draw[->] (Bad1.north).. controls +(up:1cm) and +(right:1cm) ..
(Store.east); 

\draw[->, color=blue] (Store.west).. controls +(left:1cm) and +(left:1cm) ..
(Good1.west); 

\draw[->, color=blue] (Good1.south).. controls +(down:1cm) and +(left:1cm) ..
(A1.west); 


\node (Today) at (-3,2) {Todays's mail} ;
\path[draw, ->, color=blue]  (Today) .. controls +(down:1cm) and +(left:1cm) ..
(A1.west) ;

\end{tikzpicture}
\caption{Schematic of Cinderella with multi-day store and store-sort}
\end{figure}

This run can take place during non-peak-hours, where there are no
tight time constraints. The machinery is there anyways and the
additional cost is therefore moderate.

Splitting the store into just two parts seems odd, considering that
you have sorting machines with 200 and more stackers. You can however,
sort the good mail in a way which corresponds to the output of the
main sorting run plus one additional output which contains all the bad
mail, which goes back to store.

\begin{figure}[htb!]
\centering
\begin{tikzpicture}

\tikzstyle{every path}=[very thick]
\tikzstyle{tmtape}=[draw,minimum size=0.5cm, fill=blue!20]
\tikzstyle{store}=[draw, fill=lightgray!20, minimum size=2em, minimum
width=3.5cm, rounded corners]

\node (Store) at (6,4) [store] {Store};

\begin{scope}[start chain=1 going right,node distance=-0.2mm]
 \node [on chain=1,tmtape] (A1) {A};
    \node [on chain=1,tmtape] {B};
    \node [on chain=1,tmtape] {C};
    \node [on chain=1,tmtape] {D};
    \node [on chain=1,tmtape] {$\ldots$};
    \node [on chain=1,tmtape, minimum width = 1.4cm,
    fill=lightgray!20] (Bad1) {bad};
\end{scope}

\begin{scope}[shift={(5cm,0cm)}, start chain=2 going right,node distance=-0.2mm]
 \node [on chain=2,tmtape] (A2) {A};
    \node [on chain=2,tmtape] {B};
    \node [on chain=2,tmtape] {C};
    \node [on chain=2,tmtape] {D};
    \node [on chain=2,tmtape] {$\ldots$};
    \node [on chain=2,tmtape, minimum width = 2cm,
    fill=lightgray!20] (Bad2) {bad};
\end{scope}
\node[draw, very thick, fill=blue!20] at (2.2,-4) (Out) {Good A};
\node at (3.5,-4) {$\ldots$};


%% good outputs

\draw[->, color=blue] (A1.south).. controls +(down:1cm) and +(up:1cm) ..
(Out.north);

\draw[->, color=blue] (A2.south).. controls +(down:1cm) and +(up:1cm) ..
(Out.north); 

%% bad outputs

\draw[->] (Bad1.north).. controls +(up:1cm) and +(left:1cm) ..
(Store.west); 

\draw[->] (Bad2.north).. controls +(up:1cm) and +(right:1cm) ..
(Store.east); 

\draw[->, color=blue] (Store.south).. controls +(down:1cm) and +(left:1cm) ..
(A2.west); 


\node (Today) at (-1,2) {Todays's mail} ;
\path[draw, ->, color=blue]  (Today) .. controls +(down:1cm) and +(left:1cm) ..
(A1.west) ;

%% Run lables
\node [above of = A1, fill=white] {Main sorting run};
\node [above of = A2, fill=white] {Store-sorting run};
			
\end{tikzpicture}
\caption{Schematic of Cinderella with multi-day store and fine store-sort}
\end{figure}


This way, the good mail from store will not run though the main
sorting run at all. The outputs of the main sorting run and the
store-sorting are merged later. In fact the main sorting run and
the store-sorting run "look" the same. For each stacker in the main
sorting run there is a corresponding stacker in the store-sorting run,
including the stacker for bad mail.

Usually you will run the store-sorting before the main sorting run,
i.e. the good mail from store will already be pre-sorted, when the main
sorting run starts. However, this is not a requirement. Because you
\emph{know} what is in store, you can run the main sorting run
first. The output of the store-sorting run is entirely predictable.


\begin{code}
cinderella2 :: SimDay
cinderella2 mailIn state@(g, s, d) = (g, toStore, logEntry:d)
        where
            -- separate ClassA mail from today
            (newA, newB) = splitMailBy (\mpc -> (mc mpc == 'A')) mailIn

            -- split mail in store into young and old part
            today = fromIntegral $ length d
            (young, old) =splitMailBy (\mpc -> mday mpc > (today - 3)) s

            -- find most dense mpcs 
            isDense mpc = countMatches s mpc  > 2

            -- from the young mail and today's B,  take what matches old mail
            (goodYoung, toStore) = splitMailBy 
                                   (\mpc -> containsSc old mpc 
                                            || isDense mpc) (mergeMail [newB,young])
                                                                               
            mailOut            = mergeMail [old, goodYoung, newA]
            logEntry           = (newDayLog state mailIn mailOut toStore) 
\end{code}

\section{Appendices}
\input{Mail}
\input{Plot}
%\input{MailTest}

%if False
\begin{comment}
X$a 
\end{comment}
%endif


\end{document}

