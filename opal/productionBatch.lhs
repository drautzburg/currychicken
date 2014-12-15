%include lhs2TeX.fmt
%include greek.fmt
%options ghci -fglasgow-exts

%------------------------------------------------------------
\section{Production Orders and Batches}
%------------------------------------------------------------

\subsection{Production Orders}

In the discussions about Opal concepts, we postulated the necessity
for a thing called |Production Order|. A Production Order should
contain several Workorders.

A Production Order should encompass a \emph{meaningful unit of work}
as opposed to a |Workorder| which is the smallest possible unit of
work.

Production Orders were used to do the following:

\begin{description}
\item[State reporting] The user may not be interested in the states of
  Workorders, but wants to see an aggregated state of many
  workorders. Casually speaking, the completion of a Workorder is
  nothing to write home about, the completion of a Production Order
  however is.
\item[Bulk operations] The user may want to maniplate several
  Workorders in one go. The most prominent example was delaying all
  Workorders by a certain time.
\end{description}

I believe these two things are relatively simple. All we need is a way
to select Workorders from the set of all Workorders, based on whatever
criteria appear appropriate. Except for these criteria, a Production
Order would have no attributes at all.

Furthermore, Production Orders just hardly exist in the real
world. They don't really \emph{do} anything. Even bulk operations work
better without Production Orders, because you can easily imagine two
different bulk operations, whose underlying sets of Workorders
overlap. To support this with Production Orders you would have to
allow that a Workorder can belong to more than one Production Order.

\begin{note}[Insight]
  At the end of the day a Production Order is simply a condition by
  which we can select Workorders. It can be created and destroyed
  without any impact on the system. Therefore it is questionable,
  whether |Production Order| qualifies as an Entity. I believe we
  should not give it too much attention.
\end{note}


\subsection{Batches}

While Production Orders stand for a \emph{meaningful unit of work} a
|Batch| stands for \emph{meaningful unit of workload}, i.e. a
collection of |Items|.

If Batches were input to Production Orders then one could reason about
the progress of a Production Order. A Production Order would be done
if its input Batches become empty.

However, earlier we said that the state of a Production Order can be
derived from the states of its Workorders. This does make more sense,
because even though the input Batches have become empty, some
workorder may still be processing these items. Look at the ``unpacking
complete'' example in Note~\ref{note:unpack}. Even though the input
batch (the container) was completely empty, items were still being
processed by the |trans| Workorder. If we define a Production Order,
which ends \emph{after} |trans|, then we better not declare it as
completed until \emph{all} Items were seen at the destination of
|trans|.

But even when we have a Production Order which contains only a single
Workorder we're still in trouble. What if we decide to only unpack
half of the container. Clearly the job would be done when half of the
container is unpacked. Should we create a |Batch| for \emph{half of
  the container}?

If we did that, then it will be difficult to tell what Items we expect
to be in that half. We cannot say, that from a total set of items
|1..100| we want |1..50| unpacked. The job would also be done if items
|1,3,5,..99| were unpacked. All we really ask for that half the items
are unpacked. A traditional Batch as a number of Items simply doesn't
cut it.

However, if we know where the items are, then it is a piece of cake to
answer whether half of them are unpacked. We just need to count the
Items which are still in the container. 

It appears like we really want to just count items which satisfy
certain criteria. Then Batches become just a volatile as Production
Orders and the same reasoning applies.


\begin{note}[Insight]
  A Batch is simply a condition by which we can select Items. It can
  be created and destroyed without any impact on the system. Therefore
  it is questionable, whether |Batch| qualifies as an Entity. I
  believe we should not give it too much attention.
\end{note}
