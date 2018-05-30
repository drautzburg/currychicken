%include lhs2TeX.fmt
%include greek.fmt
%options ghci -fglasgow-exts

\subsection{Rendering}
\subsubsection{Problem Statement}

Suppose we have different Players, e.g. a piano player and a guitar
player. Each of the players reads a score, which is specific to his
instrument. A piano player would read a piano score.

A player must transform his score into something that can be played on
one or more midi channels. A guitar player may require a midi channel
for each string, where a piano player may need only a single channel.

There is a limited number of midi devices, each having a number of
channels. One player must not use a device/channel combination, which
is already used by another player

\subsubsection{Clarifications}

Initially there is a collection of Devices and Channels. A Device is
identified by its name.
\begin{code}
import CommonFormatting
import Control.Monad.State
\end{code}

\begin{code}
type Device = String
type Channel = Int
type ChannelPool = [(Device, Channel)]
\end{code}

All players produce the same type of output, namely a MidiScore. We
won't define what a MidiScore is, but we'll assume it can be
transformed into an IO action.

\begin{code}
data MidiScore
playMidi :: MidiScore -> IO()
playMidi = undefined
\end{code}

The MidiScore contains all instruments, however a single player should
be unaware of the other players. A player produces a |MidiScore| but
with more than one Player, several MidiScores need to be merged into
one. Hence, a |MidiScore| needs to be a |Monoid|

\begin{code}
instance Monoid MidiScore where
    mempty  = undefined
    mappend = undefined
\end{code}

\subsubsection{A possible solution}

Consider a guitar player:

\begin{code}
data GuitarScore
guitarToMidi1 :: GuitarScore -> ChannelPool -> (ChannelPool, MidiScore)
guitarToMidi1 = undefined
\end{code}

The right part |ChannelPool -> (ChannelPool, MidiScore)| is a state
transformation, which can also be writte as

\begin{code}
guitarToMidi :: GuitarScore -> State ChannelPool MidiScore
guitarToMidi = undefined
\end{code}

With an additional piano player:

\begin{code}
data PianoScore
pianoToMidi :: PianoScore -> State ChannelPool MidiScore
pianoToMidi = undefined
\end{code}


If we have multiple players, then we'll end up with multiple |State
ChannelPool MidiScore| transitions. If the container for these is
simply a list, then we need this kind of transformation, to combine
them:

\begin{code}
type T1 = [State ChannelPool MidiScore] -> State ChannelPool [MidiScore]
\end{code}

This transformation is what |sequence| does, whose general type is

\perform{:cmd verb ":t sequence"}

In our case the Monad is |State ChannelPool|, |t| is |[]| (aka List)
and |a| is |MidiScore|.

So we can at least produce a List of MidiScores:

\begin{code}
aGuitarScore = undefined :: GuitarScore
aPianoScore  = undefined :: PianoScore
render1 :: State ChannelPool [MidiScore]
render1 = sequence $ [
    guitarToMidi aGuitarScore,
    pianoToMidi  aPianoScore  
    ]
\end{code}

With an initial |ChannelPool| we can put it all together
\begin{code}
initialPool = undefined :: ChannelPool

render2 = foldr mappend mempty (evalState render1 initialPool)
\end{code}

We'd like to have a function which takes the initial pool and a list
of MidiScores. This requires a bit of reordering
\begin{code}
render :: ChannelPool -> [State ChannelPool MidiScore] -> MidiScore
render cp scpm = foldr mappend mempty (evalState (sequence scpm) cp)
\end{code}

Now we can render and play

\begin{code}
play :: IO()
play = (playMidi . render initialPool) [
    guitarToMidi aGuitarScore ,
    pianoToMidi  aPianoScore  
    ]
\end{code}










