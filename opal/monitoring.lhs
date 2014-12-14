%include lhs2TeX.fmt
%include greek.fmt
%options ghci -fglasgow-exts

%------------------------------------------------------------
\section{Monitoring}
%------------------------------------------------------------

In our simulation we assumed that we know the initial state of the
world. The question is: how do we obtain this state?

\subsection{Imperfect sensors}

If our world is equipped with multiple sensors, then their readings
could tell us what items are where. However, this requires quite a
dense net of sensors. Our system has a conception of
\emph{interesting} locations and the sensor net would need to observe
at least all these locations. 

Furthermore we are interested in items and the sensors would have to
observe individual items. In the real world sensors are often less
precise and may just count or weigh a whole set of items as oposed to
registering them each individually.

Finally we must expect that sensor readings do not arrive in
chronological order or arrive significantly later than the event they
refer to. 


\subsection{Interpolating}


If we want to obtain the state of the world in the light of an
incomprehensive sensor net, we need to be able to interpret sensor
readings in an intelligent way and obtain the \emph{most likely} state
of the world. To do so, we may exploit our knowledge of the world.

Take for example an intem which travels from location |A| to |B| and
then to |C|. Assume we only have sensors at |A| and |C|. Thus we can
never be certain when the item is at |B|. Still there may be things
which depend on the presence of an item at |B|, so we must at least
make an educated guess.

\begin{figure}[htb!]
\centering
\includegraphics[width=8cm]{interpolating.eps}
\caption{Interpolating Mon}
\end{figure}


Without knowing the real world processes this is impossible. However,
if we know that the item travels |A -> B -> C| and we know how long
each leg takes, then we can \emph{interpolate} between sensor
readings. 

I believe, sensor readings can be categorized in the following way:

\begin{description}
\item[seen] One such message could be a scan-event, which tells us
  that an Item was \emph{seen} at a certain location at a certain
  instant. 

\item[unseen] Another message could tell us e.g. that a container is
  completely unloaded. This tells us, that there are no more items in
  the container. In contrast to the first message this does not tell
  us where items actually are. It rather tells us, where they are
  certainly \emph{not}: no item can be in that container.

  A variation of the second message is the information that all Items
  in a given container have been scanned. This tells us, that
  \emph{only} those scanned items were in the container, i.e. all
  items which were not scanned were certainly not in the container.
\end{description}

\begin{figure}[htb!]
\centering
\includegraphics[width=8cm]{unpackComplete.eps}
\caption{Locations of items after ``unpacking complete''}
\end{figure}

\ding{228} It is important to understand that an \emph{unpacking
  completed} message does \emph{not} tell us, that all items in the
container are now at the Spot where the unpacking process put them
(|Area 51|). They could well have been moved away while the unpacking
was still in progress.

|Unseen| messages will only convey information if we have the
possibility to reason like ``well, those items are no longer in that
container, so they must be in one of these other locations''. This
requires that we have an idea of where items could \emph{possibly} be.

It appears that it is not so easy to come up with a model, which is
capable of interpolating sensor readings by taking knowledge about
real world processes into account. But read
\href{http://rsif.royalsocietypublishing.org/content/9/77/3411.long}{this:}

\begin{quotation}
  If we have a continuous-time Markov chain model of a real-world
  system, then we can use the model to make predictions about the
  system. 
  \begin{itemize}
  \item For instance, we might take the steady-state probabilities of
    the chain as predictions of what we would likely see if we were to
    observe the state of the real system at some arbitrary time.
  \item Alternatively, if we knew the state of the real system at some
    time, then we could use the model to compute the probabilities of
    different possible states at future times.
  \end{itemize}
\end{quotation}

\subsection{Conclusion for now}

It turns out that 
\begin{itemize}
\item knowing what the world looks like and 
\item making predictions about how the world will evolve from there
\end{itemize}
are two closely related things.

In the course of the Opal discussion, a system which is capable of
doing the first bullet was called |Mon| (monitoring).

The second bullet stands for a simulation. It will need to know the
state of the world at the beginning of the simulation. But other than
that it has no business with the world.

Both aspects have something in common: both need to know about the
processes in the real world. A simulation uses this knowledge for
projecting an assumed state into the future and |Mon| uses this
knowledge for interpolating between sensor readings. So even |Mon|
contains an element of prediction.

Unlike a simulation, |Mon| will have to continously correct its
interpolated states by exploiting sensor readings. If the sensor net
was all-encompassing, then |Mon| could live without prediction, but
this is unlikely to be the case.

The type of |Mon| is something like this:
\begin{verbatim}
Mon :: ProcessModel -> (Time, ItemLocations) 
        -> [(Time, SensorReading)] 
        -> [(Time, ItemLocations)]
\end{verbatim}
So |Mon| takes a |ProcessModel|, a state of the world and a number of
|SensorReadings| and produces a movie of world states. 

The type of a simulation is
\begin{verbatim}
Sim :: ProcessModel 
        -> (Time, ItemLocations)
        -> [(Time, ItemLocations)]
\end{verbatim}

This is exactly like |Mon| except there are no sensor readings to
consider.

\ding{228} It appears that the difference between |Mon| and a
simulation is marginal. If |Mon| is capable of interpolating then we
can just have it interpolate into the future, i.e. beyond the latest
sensor reading and we get a simulation.

%\begin{figure}[htb!]
%\centering
%\includegraphics[width=4cm]{glass-slipper.jpg}
%\end{figure}

%World = (ItemPositions, Resources, Time)


