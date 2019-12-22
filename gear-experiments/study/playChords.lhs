%include lhs2TeX.fmt
%include greek.fmt
%options ghci -fglasgow-exts

\section{Play Chords}
\subsection{Problem Statement}

Suppose we have a given Chord progression, and we want to write a
piano part, where each Chord is played in root position on the 1 and 3
of each bar. We want to produce output, which is a stream of Notes,
each having a start time and a duration.

\subsection{Clarifications}

First, we need to clarify what we mean by "Chord progression". We
assume that throughout the song, there is always a \emph{current
  chord} and that we can ask for the current chord at any point in
time.

How we define |Time| and |Duration| is basically a matter of taste,
except that both need to be in the |Num|, |Eq| and |Ord|
classes\footnote{Times and Durations can partake in arithemtic
  operations ($+,-,abs\dots$), they can be compared for equality ($==,
  /=$) and and inequality ($>,>=,<,<=$)}.  We choose to express both
as |Rational| numbers, where
\begin{description}
\item[|Duration|] is expressed as a fraction of a whole note, such
  that |1%1| is a whole note, |1%4| is a quarter note and |3%8| is a
  dotted quarter note.
\item[|Time|] is expressed relative to some starting point, such that
  |Time=0| coincides with the starting point and |1%4| is one
  quarter note after the starting point. The "one" of the first bar
  therefore has a |Time| of 0 (assuming the starting point is not
  in the middle of a bar).
 \end{description}



%if false
\begin{code}
{-# LANGUAGE DeriveFunctor #-}
-- some boilerplate
import Data.Ratio
import Data.Foldable
import Data.Typeable
import Data.Tuple
import Data.Data
import CommonFormatting
prettyEvts evts = putStr "Events " >> (prettyL 4 3  . unEvts ) evts
\end{code}
}
%endif

Then here we go:
\begin{code}
type Time = Rational
type Duration  = Rational
\end{code}

When we want to play the \emph{current chord} at the 1 and 3 of each
bar, we are in the lucky position of already knowing \emph{when} to
play something. The problem statement did not tell us the desired
Duration, so let's just say, we play each Chord for the Duration of
|1%4|.

So, we already know, the the output will be something, that occurs at
these Times:

\begin{code}
outputTimes = [2*n%4 | n <- [0..7]] :: [Time]
\end{code}

\perform{:cmd verb "outputTimes"}

For each of these times we need to take the current chord and convert
it to the desired stream of notes.

For now, we do not want to delve into Notes, Chords, their inversions
and how all of this is represented. Instead we'll cheat a little.

The output shall be a stream of Notes. For now we define a |Note| as a
|String| with a |Duration|.

\begin{code}
data Note = Note String Duration deriving (Eq, Show)
\end{code}

And the required output will then be
\begin{code}
type Output' = [(Time, Note)]
\end{code}

We won't get too sophisticated with Chords either. To convert a
|Chord| into |Output| we define a mock function, which just produces
Notes whose String part describes in prose what Note we mean.

\needspace{12em}
\begin{code}
data Chord = Cmaj | Dmin | G7 | SomeChord deriving (Eq, Show)

renderChord :: Chord -> [Note]
renderChord c = [
    Note (show c ++ "-root")(1%4),
    Note (show c ++ "-3rd") (1%4),
    Note (show c ++ "-5th") (1%4)
    ]
\end{code}
It may not be obvious why we need |SomeChord| . The thing is, we're
gavitating towards the concept of a \emph{current chord}. Since there
must be a current chord for all times, we can use |someChord| as a
placeholder for times where we don't care about the current chord.

\needspace{24em}
\perform{:cmd verb "pretty $  renderChord Dmin"}

Since we want to render chords at certain moments, we need to know the
\emph{current chord} for each moment. An operation like "I know the
moment, you give me the chord" is essentually a function from |Time|
to |Chord|. We could model our chord progression as:

\begin{code}
chords1 :: Time -> Chord
chords1 t
    | t<0 = SomeChord
    | t<2 = Cmaj
    | t<3 = Dmin
    | t<4 = G7
    | otherwise = SomeChord
\end{code}

Alternatively, we can define a Chord-progression as a list of Chord \emph{changes}:

\begin{code}
chords2' :: [(Time, Chord)]
chords2' = [
           (0, Cmaj),
           (2, Dmin),
           (3, G7),
           (4, SomeChord)
          ]
\end{code}

With this data, we can always implement a function from |Time|.

\subsection{Functional reactive programming (FRP)}

"Function from |Time|" sounds a lot like FRP. And our |chords2'| look
a lot like Events. Let's try to tackle things with FRP methods.

\needspace{12em}
In FRP a "function from |Time|" is called a |Behavior|:

\begin{code}
newtype Behavior a = Beh {
    at :: Time -> a
} deriving Functor
\end{code}

|Output| and |chords2| are both lists of pairs whose first
component is a |Time|. Let's call these things |Events|.

\needspace{12em}
\begin{code}
newtype Events a    = Events {
    unEvts :: [(Time,a)]
    } deriving (Eq, Show, Functor)
\end{code}

So the type |Events| represents a whole stream of Events. Each Event
has a |Time| and a value part:

\begin{code}
evtTime :: (Time, a) -> Time
evtTime  = fst

evtValue :: (Time, a) -> a
evtValue = snd
\end{code}

Essentially |Events| are just wrapped up lists |[(Time, a)]|. We can
lift a function which transforms such lists to a function which
transforms |Events|:

\begin{code}
onEvts :: ([(Time, a)] -> [(Time, b)]) -> Events a -> Events b
onEvts f = Events . f . unEvts
\end{code}

And finally some more auxilary functions:

\begin{code}
-- Check if |Events| is empty
nullE :: Events a -> Bool
nullE = null . unEvts

-- Construct |Events| from a List of |Time|s
fromTimes :: (Time -> a) -> [Time] -> Events a
fromTimes f = let ft t = (t, f t)
              in (Events . map ft) 

upTo :: Time -> Events a -> Events a
upTo t = onEvts $ takeWhile ((<=t) . evtTime)
\end{code}


\head{Creating a Behavior}

We can create a |Behavior| from a default value and |Events|, such
that the value of the |Behavior| at time |t| is the value of the
latest Event that occurred before |t|. For times before the first
event we want the default value.


\begin{code}
stepper :: a -> Events a -> Behavior a
stepper a0 evts = Beh $ \t -> case upTo t evts of
                                Events [] -> a0
                                Events xs -> evtValue . last $ xs
\end{code}

\needspace{12em}
\head{Composing Events and Behaviors}

If we bring a |Behavior b| in contact with |Events e| we want to take
the value of the |Beahvior| for each |Time| an Event occurs. So for
each Event, we get an Event value |e| and a Behavior value |b|. From
these two, we can compute a new Event value, which replaces the
original Event value.

To compute the new Event value, We can e.g. provide a function:

\begin{code}
zipWithBE' :: (b->e->a) -> Behavior b -> Events e -> Events a
zipWithBE' f beh = let fx (t,v) = (t, f (at beh t) v)
                   in (onEvts . map) fx
\end{code}

However, simply |fmap|ping a function |b->e->a| over a |Behavior b|
will already give us |Behavor (e->a)| for free. Similar considerations
apply to |fmap|ping a binary function over |Events e|.

The question remains, how to proceed with a |Behavior| or |Events|
whose values are functions. Here is what we could do\footnote{The
  function |apply| belongs to the standard |FRP| vocabulary whereas
  |applyEvt| is a bit unusual. }

\begin{code}
applyEvt:: Behavior b -> Events (b->a) -> Events a
apply   :: Behavior (e->a) -> Events e -> Events a
\end{code}


We cannot implement these function with |fmap| alone. This is because
the mapped function only operates on the value part and is unaware of
the |Time| part. So, \emph{taking the value of a |Behavior| at the
time of an Event} is beyond its reach.

However, a special kind of |fmap| which maps functions with an
additional |Time| parameter, does the trick:

\begin{code}
fmapE :: (Time -> e -> a) -> Events e -> Events a
fmapE ft = let fx (t,v) = (t, ft t v)
           in (onEvts . map) fx 
\end{code}

By constructing an appropriate function |ft| we can now implement
|applyEvt|, |apply| and |zipWithBE|:

\begin{code}
applyEvt beh = let ft t f = f (at beh t) 
               in fmapE ft
                  
apply fBeh = let ft t e = (at fBeh t) e
             in fmapE ft 

zipWithBE :: (b->e->a) -> Behavior b -> Events e -> Events a
zipWithBE f = apply . fmap f 
\end{code}

\needspace{24em}
\subsection{A possible solution}

\head{FRP formulation of the Problem}

We can now rewrite our problem in terms of |Behavior| and |Events|:

Given
\begin{description}
\item[a chord progression]:
\begin{code}
chordProgE :: Events Chord
chordProgE = Events chords2'
\end{code}

\item[a rendering function]:
\perform{:cmd verb ":t renderChord"}

\item[a list of |Times|]:
\perform{:cmd verb ":t outputTimes"}  
\end{description}

produce Output of the followig type
\begin{code}        
type Output = Events Note
\end{code}

by applying the rendering function to the \emph{current chord} at
each of the |outputTime|s and collecting the results.

\head{The FRP solution}
First we create the \emph{current chord} |Behavior|
\begin{code}
currentChordB :: Behavior Chord
currentChordB = stepper SomeChord chordProgE 
\end{code}

To see it in action, we can e.g. get the |Chord| for every half note
up to the beginning of the 5th bar.

\perform{:cmd verb "map (at currentChordB ) [ 0%4, 2%4..4]"}

Next, we convert our |outputTimes| to |Events ()|

\begin{code}          
outputE :: Events ()
outputE = fromTimes (const ()) outputTimes
\end{code}

So, we have the |currentChord| Behavior and the |outputE| Events. If
we want them to interact, one of them must carry functions. This gives
us two choices:

\head{Let |Events| carry functions and use |applyEvt|}

To use |applyEvt|, we need |Events| whose values are functions
|Chord->a|. We already have a function

\perform{:cmd verb' ":t renderChord"}

However, this is a \emph{unary} function. If we want to use |fmap| to
create |Events (Chord->a)|, we need a \emph{binary} function, whose
first argument has the type of the |Events|. However, so far we have
|Events ()|, i.e. events which do not carry a payload at all. So the
function can ignore its first argument.

We can give functions an additional argument by applying |const|. 

\perform{:cmd verb' ":t const"}

Note that |a| could be a function type |x->y|. Passing such a function
to |const| creates a function with an additional first argument of
type |b|.

If we pass the resulting function to |flip|, we we get a function that
ignores its \emph{second} argument. Here we convert a function |b->c|
into a function with an additional second argument of type |b1|.

\perform{:cmd verb' ":t flip . const"}

So we can give |renderChord| an additional first argument and |fmap|
the result over |outputE|. This gives us |Events| whose values are
functions |Chord -> [Note]|

\perform{:cmd verb' ":t (fmap . const) renderChord outputE"}

\begin{code}
out1 :: Events [Note]
out1 = let fEvt = (fmap . const) renderChord outputE
       in applyEvt  currentChordB fEvt
\end{code}

\head{Let Behavior carry functions and use |apply|}

If we want to use |apply|, we must construct a |Behavior| whose values
have the type |() -> [Note]|. We can create such a |Behavior| by
|fmap|ping a function that ignores its second argument over
|currentChordB|:

\perform{:cmd verb' ":t (flip . const) renderChord"}
\perform{:cmd verb' ":t (fmap . flip . const) renderChord currentChordB"}

\begin{code}
out2 :: Events [Note]
out2 = let fBeh = (fmap . flip . const) renderChord currentChordB
       in apply  fBeh outputE
\end{code}

This produces the same result as |out1|:

\perform{:cmd verb "out1 == out2"}

\head{Implementing |applyEvt| in terms of |apply|}

Earlier we said, that |apply| is a standard FRP function, but
|applyEvt| ist not. In fact, once you have |apply| you can always
implement |applyEvt|:

\begin{code}
applyEvt':: Behavior b -> Events (b->a) -> Events a
applyEvt' = apply . (fmap . flip) ($)
\end{code}

\needspace{12em}
\head{Flattening}

Unfortunately, the result we get so far, has the type

\perform{:cmd verb' ":t out1\n:t out2"}

instead of the desired output type of

\perform{:cmd verb' ":info Output"}

but we're close. We still need a function

\begin{code}
flatten :: Events [Note] -> Events Note
\end{code}

To convert |(a,[b])| into |[(a,b)]|, we need a function like

\begin{code}
flatr :: (a, [b]) -> [(a, b)]
\end{code}

Compare this to:

\perform{:cmd verb' ":t sequence"}

If the |Traversable t| is |((,) a)|\footnote{|((,) a)| is a type
  constructor. When given a type |b|, it constructs a type |(a,b)|. It
is indeed a |Traversable|}
and the |Monad m| is |[]|, then |sequence| already does all we need
and |flatr| is not really needed.

\begin{code}
flatr = sequence  
\end{code}

Now |sequence| looks good as long as we have a single |(a,[b])|
pair. But we have a whole list of such pairs inside the output
|Events|. More spefically, we are dealing with

\perform{:cmd verb' ":t unEvts out1"}

Passing |sequence| to |concatMap| will turn it into a function which
flattens a whole list of |(a, [b])|. Finally, passing this to |onEvts|
will create a function, which operates on |Events|. This is the
function we need.

\begin{code}
flatten =  (onEvts . concatMap) sequence
\end{code}


\needspace{24em}
So here are all the transformations combined

\begin{code}
output :: Output
output  =
    let
      -- given:
      chords :: Events Chord
      chords  = Events [(0, Cmaj), (2, Dmin), (3, G7),(4, SomeChord)]

      render :: Chord -> [Note]
      render = renderChord

      outputTimes   :: [Time]
      outputTimes   = [2*n%4 | n <- [0..7]]

      -- Convert |outputTimes| to |Events ()|
      outputE :: Events ()
      outputE  = Events [(t,()) | t <- outputTimes]

      -- Express the currentChord as a |Behavior Chord|
      currentChordB :: Behavior Chord
      currentChordB = stepper SomeChord chords

      -- Let the |Events| carry a function
      fout1 :: Events () -> Events [Note]
      fout1 = applyEvt currentChordB . (fmap . const) renderChord

      -- Or alternatively, let the |Behavior| carry a function
      fout2 :: Events () -> Events [Note]
      fout2 = apply $ (fmap . flip . const) renderChord currentChordB
                        
    in flatten $ fout1 outputE
\end{code}
\needspace{24em}
Finally let's look at the Output:
\perform{:cmd verb "prettyEvts output"}

