%include lhs2TeX.fmt
%include greek.fmt
%options ghci -fglasgow-exts

%------------------------------------------------------------
\section{Workorders}
%------------------------------------------------------------
A |Workorder| is the smallest unit of work we can control
individually. Its main purpose is a transformation of |Items|. 

\begin{itemize}
\item In many cases items will just change their locations. This
  includes transportation, sorting, packing and unpacking.

\item Other transformations may change the |Product| of an item,
  e.g. by transforming a letter into a cancelled letter by running it
  through a CFC machine.
\end{itemize}


Whether or not a Workorder can do its job depends on the state of the
world. 

\begin{note}[Rule]
\label{thm:changeLoc}
A change-of-location can only take place if there are items present at
the origin location and if there is room at the destination location.
\end{note}

A Workorder needs certain |Resources| to do its job, and this brings in
a whole new aspect. If you consider a simple transport, then the
condition of Note~\ref{thm:changeLoc} may be satisfied at times and
not satisfied at other times. Still you don't want to request
resources each time you can move an item and release them when you
cannot. Chances are you would spend a lot of effort for chasing people
around instead of getting the job done.

Instead, we'd rather request Resources for an extended period of
time. Then however, we better make sure these resorces have work to do
and don't just sit around idle most of the time. This means that a
workorder may chose not to do anything even though the conditions in
Note~\ref{thm:changeLoc} are satisfied, because it anticipates, that
not enough work has piled up to justify the binding of resources.

\begin{figure}[htb!]
\centering
\includegraphics[width=10cm]{workorderStates.eps}
\caption{Workorder States}
\end{figure}

So a workorder can be in one of these states

\begin{description}
\item[Inactive-idle] Not enough work has piled up to justify binding
  resources.
\item[Inactive-busy] There is enough work and we try to bind resources.
\item[Active-idle] Resources are available, but there is currently
  nothing to do (e.g. according to Note~\ref{thm:changeLoc})
\item[Active-busy] There is something to do and resources are
  available. Transform an item.
\end{description}

