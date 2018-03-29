%include lhs2TeX.fmt
%include greek.fmt
%options ghci -fglasgow-exts

\subsection{Play Chords}
\subsubsection{Problem Statement}

Suppose we have a given Chord progression, and we want to write a
piano part, where each Chord is played in root position on the 1 and 3
of each bar. We want to produce output, which is a stream of Notes,
each having a start time and a duration.

\subsubsection{Clarifications}

First, we need to clarify what we mean by "Chord progression". We
assume that throughout the song, there is always a \emph{current
chord} and that we can ask for the current chord at any point in time.

First we define |Time| in a way which respects the way composers
think. The problem statement suggests, that we need a way to express
|StartTime| and |Duration|. We express both as |Rational| numbers.

|Duration| is expressed as a fraction of a whole note, such that |1%1|
is a whole note, |1%4| is a quarter note and |3%8| is a dotted quarter
note.

|StartTime| is expressed relative to some starting point, such that
|StartTime=0| coincides with the starting point and |1%4| is one
quarter note after the starting point. The "one" of the first bar
therefore has a |StartTime| of 0 (assuming the starting point is not
in the middle of a bar).

First some boilerplate
\footnotesize
\begin{code}
import Data.Ratio
import Data.Foldable
import CommonFormatting
\end{code}
\normalsize

Then here we go:
\begin{code}
type StartTime = Rational
type Duration  = Rational
\end{code}

When we want to play the \emph{current chord} at the 1 and 3 of each,
we are in the lucky position of already knowing \emph{when} to play
something. The problem statement did not tell us the desired Duration,
so let's just say, we play each Chord for the Duration of |1%4|.

So, we already know, the the output will be something, that occurs at
these Times:

\begin{code}
outputTimes = [2*n%4 | n <- [0..7]]
\end{code}

\perform{:cmd verb "take 4 outputTimes"}

For each of these times we need to take the current chord and convert
it to the desired stream of notes.

For now, we do not want to delve into Notes, Chords, their inversions
and how all of this is represented. So we'll cheat a little.

The output shall be a stream of Notes. For now we define a |Note| as a
|String| with a |Duration|.

\begin{code}
data Note = Note String Duration deriving (Eq, Show)
\end{code}

And the required output will then be
\begin{code}
type Output = [(StartTime, Note)]
\end{code}

We won't get too sophisticated with Chords either. To convert a
|Chord| into |Output| we define a mock function, which just produces
Notes whose String part describes in prose, what Note we mean.

\begin{code}
data Chord = Cmaj | Dmin | G7 | SomeChord deriving (Eq, Show)

renderChord :: Chord -> [Note]
renderChord c = [
    Note (show c ++ "-root")(1%4),
    Note (show c ++ "-3rd") (1%4),
    Note (show c ++ "-5th") (1%4)
    ]
\end{code}

\needspace{24em}
\perform{:cmd verb "pretty $  renderChord Dmin"}

\subsubsection{A possible solution}

We still haven't found a representation of the Chord Progression
itself. Let's assume we want |Cmaj| to last for two bars and |Dmin|
and |G7| for one bar each.

There are several ways to express this. We can e.g. define the
\emph{current chord} as a function from Time.

\begin{code}
chords1 :: StartTime -> Chord
chords1 t
    | t<0 = SomeChord
    | t<2 = Cmaj
    | t<3 = Dmin
    | t<4 = G7
    | otherwise = SomeChord
\end{code}

Alternatively, we can define a Chord-progression as a list of Chord \emph{changes}:

\begin{code}
chords2 :: [(StartTime, Chord)]
chords2 = [
    (0, Cmaj),
    (2, Dmin),
    (3, G7),
    (4, SomeChord)
    ]
\end{code}

This representation can be converted into a function from
time. 

Let's try to tackle things with FRP methods. First we define a type
|Behavior| which is essentially a function from |StartTime|

\begin{code}
data Behavior a = Beh {
    at :: StartTime -> a
}
\end{code}

Next, we notice that |Output| and |chords2| are both lists
of pairs, where the first component is a |StartTime|. Let's call these
things |Events|.

\begin{code}
type Events a    = [(StartTime,a)]
\end{code}

\needspace{8em}
Then we can convert |chords2| into a |Behavior|

\begin{code}
stepper :: a -> Events a -> Behavior a
stepper a0 evts = Beh $ \t -> case takeWhile ((<=t) . fst) evts of
    [] -> a0
    xs -> snd $ last xs

chordB = stepper SomeChord chords2 :: Behavior Chord
\end{code}

and we can e.g. get the |Chord| for every half note up to the
beginning of the 5th bar.

\perform{:cmd verb "map (chordB `at`) [ 0%4, 2%4..4]"}

But we're not done yet. We still must produce output. Let's write the
|outputTimes| in terms of |Events|. Since we only have times, but no
associated values ("payload"), the type will have to be |Events ()|.

\begin{code}
outputEvents :: Events ()
outputEvents = map (\t -> (t,())) outputTimes
\end{code}

The |apply| function of FRP looks interesting:

\begin{code}
apply   :: Behavior (a -> b) -> Events a -> Events b
apply fB [] = []
apply fB ((t,a):xs) = let f = fB `at` t
                      in (t,f a) : apply fB xs
\end{code}

If our |chordB| wasn't just about chords, but about functions which
render chords, we'd be almost there. To replace chords inside a
|Behavior Chord| with functions (or anything else), Behavior must be a
|Functor|.

\begin{code}
instance Functor Behavior where
    fmap f (Beh beh) = Beh $ \t -> f (beh t)
\end{code}

Now we can replace the |Chord| payload by a function which takes the
payload of an outputEvent, namely |()| and produces |Note|s

\begin{code}
chordRenderB :: Behavior (() -> [Note])
chordRenderB = fmap (const . renderChord) chordB
\end{code}

\needspace{24em}
Now, we can use |apply|

\perform{:cmd verb "prettyL 1 3 $ apply chordRenderB outputEvents"}


This is not quite what we wanted. We wanted |[(StartTime, Note)]|, but
we got |[(StartTime, [Note])]|. In other words: we wanted |Events
Note]| but we got |Events [Note]|. We need to flatten the structure.


\begin{code}
flatten1 :: Events [a] -> Events a
flatten1 evts = do
    (t1,ns) <- evts
    zip (repeat t1) ns
\end{code}

\needspace{12em}
So here are all the transformations combined

\begin{code}

output :: Output
output  =
    let chords       = [(0, Cmaj), (2, Dmin), (3, G7),
                        (4, SomeChord)]                  :: Events Chord
        chordB       = stepper SomeChord chords          :: Behavior Chord
        chordRenderB = fmap (const . renderChord) chordB :: Behavior (() -> [Note])
        outputEvents = [(2*n%4,()) | n <- [0..7]]         :: Events ()
        nestedOutput = apply chordRenderB outputEvents   :: Events [Note]
    in flatten1 nestedOutput                             :: Output
\end{code}

\perform{:cmd verb "prettyL 3 3 $ output"}

