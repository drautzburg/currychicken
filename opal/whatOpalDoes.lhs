%include lhs2TeX.fmt
%include greek.fmt

%------------------------------------------------------------
\section{What Opal does}
%------------------------------------------------------------
\begin{figure}[htb!]
\centering
\includegraphics[width=8cm]{whatOpalDoes.eps}
\end{figure}

Opal maintains \emph{movies} of states, where states include
\begin{description}
\item[Material State] what items are where and when. 
\item[Workorder State] what workorders are in which of the fours
  states and when
\item [Resource State] what the resources are doing, i.e. what
  Workorders they are working for or if they are available.
\end{description}

These States influence each other. A changed Material State may
acivate or deactivate Workorders and Workorders change the future
Material State by moving Material or intending to do so.

The Resource State affects Workorders, because Workorders may not be
able to become active if not all Resources are available. Workorders
affect Resources, because they bind Resources when they become active
and they release Resources when they become inactive. Hence every new
Workorder State is usually associated with a new Resource State.

\begin{figure}[htb!]
\centering
\includegraphics[width=12cm]{stateDependencies.eps}
\end{figure}

Both the Material State and the Resource State may change due to
messages from the real world. Personnel may badge in or out, leading
to more or fewer staff being available (red circle).

The (assumed) Material State can change due to sensor readings as
discussed earlier.

The Workorder States are affected by the rules which control their
activation or deactivation and the required resources. All of this is
set up by Planners.

Finally Opal knows about the (wallclock) time in the real world. This
affects Opal's idea of past and future and affects what it is willing to
change about its view of the world. Changing the past is possible, but
requires special attention.

Of course there is a \emph{current State}, i.e. the State at
$t=t_{wallclock}$, but this is hardly a distinguished State. You can
ask for the State at any Point in time, past, present or future. Also
we cannot assume that the current and past states are known
precisely. They may be known with a higher certainty than future
states though.

\subsection{Comparison to current approach}

At first glance the idea of sate movies appears weird. But at a closer
look it turns out that this is well in line with our current
``traditional'' approach.

\begin{figure}[htb!]
\centering
\includegraphics[width=10cm]{opalTimeline.jpg}
\caption{An existing state movie}
\end{figure}

Our GUIs quite frequently show things in chronological order. This is
exactly the same as a state movie. This movie is contiously being
updated due to inputs from the real world and inputs from the
user. Also currently we do not hesitate to let this movie run into the
future (think: planned Production Orders).

My approach only differs insofar as it makes this paradigm the core of
Opal.

Another difference is the degradation of |Production Orders| and
|Batches|, which are quite prominent in the traditional approach.

