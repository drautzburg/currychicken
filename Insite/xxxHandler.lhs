%include lhs2TeX.fmt
%include greek.fmt
%options ghci -fglasgow-exts

\subsection{Handler}
%if False
\begin{code}
module Handler where
import Time
import qualified Data.Heap as H
\end{code}
%endif

The |HandlerState| consists of two event-queues. One is for internal
events, which drive the Simulation and the other if for external
event, which may be sent to antother Simulation. Finally the
|Domain| itself is part of the HandlerState.

\begin{code}
data HandlerState e d = Hds {
  iev :: TimedQ e, -- internal
  xev :: TimedQ e, -- external
  dom :: d
  }
\end{code}

A |Handler| takes an |Event| and the old |HandlerState| and produces
a new HandlerState and a new version of itself. The latter is
required, so a Handler can rember things, i.e. the next time it will
be ready to process an |Item|

\needspace{10em}
\begin{code}
data Handler e d = Hdr {
  runHandler :: Timed e -> HandlerState e d
             -> (HandlerState e d, Handler e d)
  }

\end{code}

