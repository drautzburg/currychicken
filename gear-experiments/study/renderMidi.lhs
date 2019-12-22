%include lhs2TeX.fmt
%include greek.fmt
%options ghci -fglasgow-exts

\section{Rendering MIDI}
\subsection{Problem Statement}
In chapter \emph{\ref{sect:Rendering} \nameref{sect:Rendering}} we
said that \emph{\txtmidiscore}, but other than that, the type
|MidiScore| remained a hypothetical, unspecified type. We will now
rectify this omission.

In theory, we could just roll our own type. However, it would be much
better to use a type, which is already used by existing Haskell
packages doing MIDI-related stuff.

Two prominent MIDI packages are
\begin{itemize}
  \item |alsa-seq| (exports |Sound.ALSA.Sequencer...|), which provides
    access to the |Alsa| api for sending and receiving MIDI events,
    and
  \item |midi| (exports |Sound.MIDI...|), which allows reading and
    writing MIDI files.
\end{itemize}

The types in |alsa-seq| reflect the types in the native |Alsa|
api. This does not look like a good choice for |MidiScore|, because
|Alsa| is just one of many apis. It is too specific to be used as
the common denominator of all our \emph{Players}.

The types in the |midi| package are more general and not related to a
specific api. This looks like a much better choice, not least because
is enables us to test the output side of our program with some
MIDI-file, i.e. before we have actual players to produce such data.            
                       
\subsection{Clarifications}

%if false
\begin{code}
import CommonFormatting
\end{code}
%endif

\begin{code}
import qualified Sound.MIDI.File as Mf
import qualified Sound.MIDI.File.Load as Ml
import qualified Sound.MIDI.File.Save as Ms
import qualified Data.EventList.Relative.TimeBody as TimeBody
-- import qualified Data.EventList.Absolute.TimeBody as TimeBodyA
import qualified Sound.MIDI.File.Event as Evt
import qualified Sound.MIDI.Message.Channel as Ch
import qualified Sound.MIDI.Message.Channel.Voice as ChVoice
import qualified Sound.MIDI.Message.Channel.Mode as ChMode
import qualified Sound.MIDI.Controller as Ctrl


import qualified Sound.MIDI.File.Event.Meta as Meta
import qualified Sound.MIDI.File.Event.SystemExclusive as SystemExclusive
import Numeric.NonNegative.Class
\end{code}

|Sound.MIDI.File.Load| contains the following function:

\perform{:cmd verb' ":t Ml.fromFile"}
\needspace{12em}
The returned type |Sound.MIDI.File.T| has the following constructor:

\perform{:cmd ctors ":info Mf.T"}

where

\perform{:cmd ctors ":info Mf.Type"}
\perform{:cmd ctors ":info Mf.Division"}                   

In the MIDI file-format, |Type| is called |format| and |Division| is called
|division|\footnote{See:\url{http://www.music.mcgill.ca/~ich/classes/mumt306/StandardMIDIfileformat.html}}:

\begin{sfquote}
  \def\xangled#1{\textlangle #1\textrangle}

  \xangled{Header Chunk} = \xangled{chunk type} \xangled{length}
  \xangled{format} \xangled{ntrks} \xangled{division}

  The first word, \xangled{format}, specifies the overall organisation
  of the file. Only three values of \xangled{format} are specified:
  \begin{description}
  \item[0] -the file contains a single multi-channel track
    
  \item[1] -the file contains one or more simultaneous tracks (or MIDI outputs)
    of a sequence

  \item[2] -the file contains one or more sequentially independent
    single-track patterns
  \end{description}
  The third word, \xangled{division}, specifies the meaning of the
  delta-times. It has two formats, one for metrical time, and one for
  time-code-based time:

  \begin{description}
  \item[ticks per quarter-note]the number of delta time "ticks" which
    make up a quarter-note. For instance, if division is 96, then a
    time interval of an eighth-note between two events in the file
    would be 48.
  \item[negative SMPTE format, ticks per frame] delta times in a file
    correspond to subdivisions of a second, in a way consistent with
    SMPTE and MIDI Time Code.
  \end{description}
\end{sfquote}           

So, these are technicalities. Unsurprisingly, the music appears to be
in the |tracks|.
\needspace{12em}
\head{Event lists (TimeBody)}
|Mf.Track| is just an alias for |TimeBody.T Mf.ElapsedTime
Evt.T|.

|TimeBody| is exported by
 |Data.EventList.Relative.TimeBody| from the |event-list|
 package\footnote{See: \url{http://hackage.haskell.org/package/event-list-0.1.2/docs/Data-EventList-Relative-TimeBody.html}}.
 
 Its documentation says:

\begin{sfquote}
  Event lists starting with a time difference and ending with a body.
\end{sfquote}   

There are 10 different types of event-lists in that package. They are
all list-like, where each list-element has a |time| and a |value|
component.  In our case the list type is |TimeBody| and it is the
\emph{relative} variety, not the \emph{absolute} variety, which also
exists. This means:
\begin{itemize}
\item |time| is interpreted as a time-increment relative to the
  previous element
  \item Within a |track|, the type of |time| is |Mf.ElapsedTime|,
    which is an alias for\\ |Numeric.NonNegative.Wrapper.Integer| from
    the |non-negative| package.
    \begin{sfquote}
      A type for non-negative numbers. It performs a run-time check at
      construction time (i.e. at run-time) and is a member of the
      non-negative number type class C.
    \end{sfquote}
    The above-mentioned |class C| is defined in
    |Numeric.NonNegative.Class| from the same package.
    \begin{sfquote}
      A type class for non-negative numbers. Prominent instances are
      Numeric.NonNegative.Wrapper and peano numbers. This class cannot
      do any checks, but it let you show to the user what arguments
      your function expects.
    \end{sfquote}
    \needspace{18 em}\perform{:cmd verb "3 - 2 :: Mf.ElapsedTime"}
    \needspace{18 em}\perform{:cmd verb "2 - 3 :: Mf.ElapsedTime"}
\end{itemize}

Because |time| is always a delta-time there is no question whether or
not the elements have to be sorted by time.

There are a number of ways to add elements to an event list, e.g. this
one:

\perform{:cmd verb' ":t TimeBody.fromPairList"}

The value part of the elements in a |TimeBody| is |Sound.MIDI.File.Event.T| aka |Evt.T|,

\perform{:cmd ctors ":info Evt.T"}     

So |Evt.T| is a union type which unites three event types.
              
\head{Channel.T}              
                              
\perform{:cmd ctors ":info Ch.T"}

A channel and a message body sound about right for a channel message. However that:

\perform{:cmd ctors ":info Ch.Channel"}  

looks daunting for a MIDI channel, which is really just a number
between 0 and 15. The documentation says about |Channel|:
\begin{sfquote}
  data Channel ... This definition should be in Message.Channel, but this results in a cyclic import.
\end{sfquote}

But luckily, we don't have to dig into the parser packages or import
one of them, because |Channel| offers these functions:

\perform{:cmd resp ":t Ch.fromChannel"}
\perform{:cmd resp ":t Ch.toChannel"}      

\head{Ch.Body}
                             
\perform{:cmd ctors ":info Ch.Body"}

The |Voice| bodies hold the interesting stuff

\perform{:cmd ctors ":info ChVoice.T"}             

\needspace{12em}       
The |Mode| bodies in contrast, are comparingly simple

\perform{:cmd ctors ":info ChMode.T"}         
              
\subsection{A possible solution}

Since there is nothing to implement, because everything is already
definded in the |midi| package, we will try to construct some track
data here. We start at the "bottom" and construct some voice messages.

\perform{:cmd resp ":t ChVoice.NoteOn"}

The constructors of |Pitch| and |Velocity| are not exported. Instead
there are functions to create these values from |Int|s.

\begin{code}
noteOn1 :: Int -> Int -> ChVoice.T
noteOn1 p v = ChVoice.NoteOn (ChVoice.toPitch p) (ChVoice.toVelocity v)

noteOff1 :: Int -> ChVoice.T
noteOff1 p = ChVoice.NoteOff (ChVoice.toPitch p) (ChVoice.toVelocity 0)
\end{code}

|ProgramChange| follows the same pattern
\begin{code}
programChange1 :: Int -> ChVoice.T
programChange1 prg = ChVoice.ProgramChange (ChVoice.toProgram prg)
\end{code}


For a Controller, we need to specify which controller we mean and the
value we want to set it to. The value turns out to be just an
|Int|. For the controller itself, we can either use
|ChVoice.toController|, |Ctrl.fromInt| or one of the predefined
controller names in |Ctrl|
      
\perform{:cmd resp ":t ChVoice.Control"}
\begin{code}
controller1a, controller1b :: Int -> ChVoice.ControllerValue -> ChVoice.T
controller1a ct cv = ChVoice.Control (ChVoice.toController ct) cv
controller1b ct cv = ChVoice.Control (Ctrl.fromInt ct) cv
mainVolume1 v = ChVoice.Control Ctrl.volume v
\end{code}

Next, we add the MIDI channel to get |Channel.T| and wrap it inside a
|Evt.T|. 
\begin{code}
forChannel :: Int -> ChVoice.T -> Evt.T
forChannel ch = Evt.MIDIEvent . Ch.Cons (Ch.toChannel ch) . Ch.Voice
\end{code}      

We combine this with the functions, we already have

\begin{code}
noteOn ch p         = forChannel ch . noteOn1 p 
noteOff ch          = forChannel ch . noteOff1 
programChange ch    = forChannel ch . programChange1 
controller ch ct    = forChannel ch . controller1a ct 
mainVolume ch       = forChannel ch . mainVolume1 
\end{code}

Finally we have to add times to get a |TimeBody| aka a |Track|
\begin{code}
track1 :: Mf.Track
track1 = TimeBody.fromPairList[
          (0, programChange 0 12),
          (0, mainVolume 0 64),
          (0*96, noteOn 0 60 100),
          (1*96, noteOn 0 64 100),
          (1*96, noteOn 0 67 100),
          (4*96, noteOff 0 60),
          (0*96, noteOff 0 64),
          (0*96, noteOff 0 67)
         ]
\end{code}

We should now be able to write a MIDI file with little effort. When we
stick to |Mixed| and 96 ticks, then the function to write tracks is



\begin{code}
toFile :: FilePath -> [Mf.Track] -> IO ()
toFile name = Ms.toFile name . Mf.Cons Mf.Mixed (Mf.Ticks 96)
\end{code}

Now we can save our track
        
\begin{code}
exampleMidiWrite :: IO ()
exampleMidiWrite = toFile "exampleMidiWrite.mid"  [track1]
\end{code}
