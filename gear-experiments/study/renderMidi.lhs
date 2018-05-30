%include lhs2TeX.fmt
%include greek.fmt
%options ghci -fglasgow-exts

\subsection{Rendering for Guitar}
\subsubsection{Problem Statement}

We discussed how to render in general, using multiple Players, but we
haven't discussed how to actually do this. Lets lay this out for a
guitar player.

A guitar player will need to consider each string individually. So
let's focus on a single string for starters. A player will receive
instructions what to do with that string. These instruction will have
to be translated into Midi.

\subsubsection{Clarifications}

We choose the |Sound.MIDI| module to represent MIDI data.

\footnotesize
\begin{code}
import CommonFormatting
\end{code}
\normalsize

\begin{code}
import qualified Sound.MIDI.File as Mf
import qualified Sound.MIDI.File.Load as Ml
import qualified Data.EventList.Relative.TimeBody as Tb
import qualified Sound.MIDI.File.Event as Evt
\end{code}

This package contains the following function:

\perform{:cmd verb  ":t Ml.fromFile"}

It is a function like |getLine| except it does not return a |String|
but a |Mf.T|, aka |Sound.MIDI.File.T|. This type has the following
constructor:

\perform{:cmd verb ":info Mf.T"}

The |Cons| constructor has the following type:

\perform{:cmd verb ":t Mf.Cons"}

|Track| looks interesting:

\perform{:cmd verb ":info Mf.Track"}

with

\perform{:cmd verb ":info Evt.T"}

\subsubsection{A possible solution}

\perform{:cmd verb ":info Evt.T"}









