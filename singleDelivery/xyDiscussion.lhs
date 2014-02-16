\subsection{Discussion}
This is a very small improvement over the vanilla approach. Even if we
consider the large number of addresses (tens of millions), the overall
savings will disappointing.

You might be tempted to believe, that adding yesterday's Class-B
letters for 50\% of the addresses reduces the number of single-letter
deliveries, because with more mail, the chance to receive more than
one letter is increased. While this is technically correct, this
effect is outweighed by addresses where yesterday's mail contrilbutes
the \emph{only} letter, thereby \emph{causing} a new single-letter
delivery.

As long as we operate in the $\lambda < 1$ region, more letters tend
to cause more single-letter deliveries.

You can also see this effect in equations (\ref{xpct}) and
(\ref{ypct}). The added letters in the $x$-part actually lead to an
\emph{increase} in single-letter deliveries. The only positive effect
comes from the $y$ addresses, where fewer letters decrease the
probability for single-letter deliveries.


\begin{figure}[htb!]
\centering
\begin{tikzpicture}[domain=0:2, scale=4]
\draw[very thin,color=gray] (0,0) grid (2,1);
\draw[thick, color=red] plot[smooth] (\x,{2*\x*exp(-\x)}) node[right]{$P_{\lambda}(1)$};
\draw[->, very thick] (0.4,0.536) -- (0.6,0.658) node [above]{worse $(x)$};
\draw[->, very thick] (0.4,0.536) -- (0.2,0.327) node [below]{better $(y)$};
\draw (0.6,0.658) -- (1.1,0.658) ; %upper
\draw (0.2,0.327) -- (1.1,0.327) ; %lower
\draw (0,0.491) -- (1.1,0.491) node [right]{average};
\draw (0.4,0.536) -- (0,0.536) node [left] {improvement};
\foreach \x in {0,0.5,1,1.5}
    \draw (\x,0) -- (\x,-0.1) node[below] {$\x$};
\path node at (1.8,-0.15) {$\lambda$};

\end{tikzpicture}

\caption{{XY and the Poisson distribution}\label{fig:Poisson distribution}}
\end{figure}


So XY brings a negative effect by adding more letters to the $x$ part
and a positive effect to the the $y$ part by removing letters. We only
get an overall benefit, because the positive effect outweighs the
negative effect. This again is owed to the curvature of the poisson
distribution. The "better" arrow is slightly steeper than the "worse"
arrow. 

Note that the volume fluctuations are determined by the percentage of
Class-B mail. If we had more that a mere 25\% of Class-B mail, then
both arrows would be longer, the curvature would have more effect and
the overall improvement would be larger.
