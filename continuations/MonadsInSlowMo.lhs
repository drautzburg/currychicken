\documentclass[a4paper]{article}
%include lhs2TeX.fmt
%include greek.fmt
%options ghci 

\usepackage{tikz}
\usetikzlibrary{arrows,mindmap,backgrounds}
\usepackage{fancyhdr}
\usepackage{needspace}
\usepackage{amsmath}
\usepackage{graphicx}
\usepackage{capt-of}
\usepackage{ragged2e}
\usepackage[parfill]{parskip}
\usepackage{ctable}
\usepackage{color}
\usepackage{colortbl}	
\usepackage{pgf}
\usepackage[framemethod=tikz]{mdframed}
\newmdenv[skipabove=1em, frametitle=Running it,backgroundcolor=gray!05,roundcorner=2pt]{run}

% --------------
% tikz
% --------------
%\usepackage{tikz}
%\usetikzlibrary{calc} 
%\usetikzlibrary{decorations}
%\usetikzlibrary{plotmarks}
%\usetikzlibrary{arrows}
%\usetikzlibrary{chains,fit,shapes}

\usepackage{hyperref}
\hypersetup{colorlinks=true, linkcolor=blue, pdftoolbar=true}
\author{Martin Drautzburg}
\title{Monands in Slow Motion\\\bigskip\small{Understanding Monads in small steps}}
\begin{document} \maketitle

\begin{abstract}
\end{abstract}


\tableofcontents 
\listoffigures

\section{From imperative to lazy pure functional}
In imperative languages we're used to seeing code like this

\begin{verbatim}
 student = new student();
 student.setId(12);  
 student.getId();
\end{verbatim}

The individual lines of code in this example are called
\emph{statements}. Writing a statement below another statement is a
one mechanism for composing larger programs from smaller pieces. There
are other mechanism for composing things, and the selection of
available mechanisms pretty much determines the flavor of a perticular
programming language as procedural, obeject-oriented or functional. 

The term \emph{statement} describes a syntax-aspect of the
language. But what is the meaning of a statement? Statements are said
to \emph{do} something. A statement can return a value. But some
statemens do not return a value but are used merely because of their
side-effects. I/O is a typical example, the use of setters in OO
languages is another one. A sequence of statement often manipulates a
state as in the example above.

But most importantly statements are \emph{executed} sequentially,
unless a different control-flow is explicitly expressed as in
\emph{if-then-else} or \emph{while} statements. All in all statements
come with very few guarantees, if any. This makes it difficult to
reason about imperative programs.

In a lazy pure functional language like haskell, there are no
statements. There is not much \emph{doing} going on in these
languages. \emph{Doing} in every day life mostly means altering the
state of the world, whereas in haskell you try to reduce such
activities as much as possible and you favor defining and calling
functions that compute an output value from its paramaters
\emph{without} changing the world or anything else on the way.

Let's illustrate this with a function |rootlen| that computes the
square root of the length of a string. To do this, we must take the
length of a string, which is an |Int|, convert that to a floating
point number and then pass it to a function which computes the square
root.

\begin{code}
import Control.Category hiding((.),id)

rootlen1 s = let li = length s
                 lf = fromIntegral li
                 r  = sqrt lf
             in r
\end{code}

The lines in the |let| block are layed out in the order in which they
need to be executed. But this is by no means mandatory. This version
of |rootlen| is just as valid, even though the lines are reversed.

\begin{code}
rootlen2 s = let r = sqrt lf
                 lf = fromIntegral li
                 li = length s
             in r
\end{code}

Now look at this:

\begin{code}
rootlen3 s = let li = length s
                 lf = fromIntegral li
                 m  = launchMissiles lf
                 r = sqrt lf
             in r
        where
            launchMissiles = undefined
\end{code}

In an imperative program you would expect that before calling |sqrt|
you call a function |launchMissiles|, which we left undefined due to
the lack of any real missiles. We can still tell whether, with a
proper implementation of this function, missiles would be launched.
Any attempt to call an undefined function will raise an error. But
this doesn't happen.

\needspace{8em}
\begin{run}
        |*Main> rootlen3 "love"|
       \perform{rootlen3 "love"}
\end{run}

The function|launchMissiles| never get called because its result |m|
is not needed to compute the final result, pretty much like a
reasonable compiler would not bother to call the |sin| funcion in an
arithmetic expression like |10 + 0*(sin(4))| or in a conditional like
|2>0 ? 2 : sin(4)|

Gluing together lines of codes as a method for composition in
languages like haskell looks questionable at least. It is actually
impossible, because functions are all you have and computing a return
value is as close as you can get to \emph{doing}
something.\footnote{Of course a programm eventually needs to change
  the world, because otherwise it would keep all its results to
  iteself. The |main| function in haskell has the type |IO()| which is
  pretty much equivalent to |world->world|, i.e. a function which
  changes the world. Writing a haskell program means computing a
  \emph{value} of type |IO()|}


\section{Means to compose}

So how can we compose things is haskell?

\subsection{Passing on the return value}\label{passingValues}
The most obvious way is to take the return value of one function and
pass it to another function. In a way this is what we did in
|rootlen|, which could also be written as

\begin{code}
rootlen4 :: String -> Double
rootlen4 = sqrt . fromIntegral . length
\end{code}

\begin{run}
        |*Main> rootlen4 "love"|
       \perform{rootlen4 "love"}
\end{run}

The compositional work is acomplished by means of another higher-order
function, which in infix notation looks like a dot. It's type is:

\begin{run}
        |*Main> :type (.)|\\
        \tt{\perform{:t (.)}}
\end{run}

i.e. it takes two functions as arguments and returns a function. The
first is a function from |b->c| and the second is from |a->b|, where
a,b and c can stand for any type. The leftmost function argument is
called last as in math.

$sqrt(fromIntegral(length(s)))$

You can do this in haskell too, but composing functions without
parentheses and without explicitly passing an argument
\footnote{This style is called \emph{point-free}, because the single
  argument to a function is also called the \emph{point}. You may talk
  about the value of the function at a particular point, particularly
  when you're plotting a function} is often preferred as it is less
noisy.

If you don't like the order of function being backwards, you can use
|(>>>)| from the Control.Category package and write

\begin{code}
rootlen4a :: String -> Double
rootlen4a = length >>> fromIntegral >>> sqrt
\end{code}

Two things are noteworthy:

\begin{itemize} \label{alike}
\item We compose a bigger \emph{composite} thing from smaller
  \emph{component} things and all things are alike. The smaller things
  are single-parameter functions and so is the bigger thing.
\item The operations |.| and |>>>| are associative, i.e. this is
  always true:\\
  |(f . g) . h = f . (g . h)|.
\end{itemize}

Adding two numbers results in another number. Without this, we would
lose many of the nice properties of addition, like associativity. In
contrast, the scalar product of two vectors is not a vectory anymore
and this makes ita bit hard on the wrist. There is absolutely no hope
to ever see something like this:

$(\vec{a} \cdot \vec{b}) \cdot \vec{c} = \vec{a} \cdot (\vec{b} \cdot \vec{c})$

This is because $(\vec{a} \cdot \vec{b})$ is not a vector anymore and
cannot partake in a scalar product. Take this as an indication, that
it is good thing when the composite at lease resembles its components.

\subsection{Calling a continuation}

In chapter \ref{passingValues} we described how functions can be
composed by passing a return value on to another function. This
mechanism is primarily under the control of the caller.

However you can also instruct a function to call this next function
itself, provided you tell it which function to call.

Our |rootlen| functions have the type |String -> Double|. |Double| is
the final result. The first function |length| returns an
|Int|. This |Int| is passed to the remainder of the computation, which
therefore must have the type |Int->Double|, because we have an
intermediate result of type |Int| and in the end we want a |Double|. 

We can rewrite the three functions we used to compose |rootlen| as follows:

\begin{code}
lengthC1 :: String -> (Int->Double) -> Double
lengthC1 s k = let li = length s
               in k li
\end{code}

In addition to the |String| argument, this function aceepts a second
argument |k|. Instead of returning a value right away, this function
passes its result to |k| which stands for \emph{the rest of the
computation}. The result of this call is the result of the function
itself.

This |k| argument is called a \emph{continuation} and a function which
takes a continuation is called a function in
continuation-passing-style (CPS).

While an ordinary function takes an argument of some type |a| and
produces a result of some type |r| und this has a type like |a -> r|,
a CPS function has a type like

|a -> (b -> r) -> r|

So there are three types involved, namely 
\begin{itemize}
\item the type of the input argument |a|
\item the type of the final result |r| and 
\item the type of an intermediate value |b|. 
\end{itemize}

Just like an ordinary |a -> b| function, a CPS function is mainly
concerned about computing |b| from |a|. But in a way it is also aware
of the type of the end result |r|. In contrast, an ordinary function
has no idea what its return value will be used for.

For |rootlen|, the continuation of |lengthC1| will \emph{not} be
|fromIntegral|, because |fromIntegral| is not the full rest of the
computation, but only one of two steps. We'll see about that.

First let us convert the other two functions to CPS:

\begin{code}
fromIntegralC1 :: Int -> (Double->Double) -> Double
fromIntegralC1 i k = let lf = fromIntegral i
                     in k lf

sqrtC1 :: Double -> (Double->Double) -> Double
sqrtC1 d k = let r = sqrt d
             in k d
\end{code}

When composing |rootlen|, |sqrtC| ends the computation and would not
really need a continuation. But we give it a continuation parameter
notheless, to make it reusable in other contexts, where it is not the
last function in a chain.

We compose |rootlen| from the tail end by passing |id| to |sqrtC1| as
its continuation. The identity function |id| just returns its argument
unchanged. Passing |id| as the continutaion means that the
intermediate value which was computed by the function \emph{is} the
final result and can be returned unchanged.

\begin{code}
f3 :: Double->Double
f3 d = sqrtC1 d id
\end{code}

So basically |f3| is just a plain |sqrt| derived from the CPS function
|sqrtC1|.

Now |f3| is the rest of the computation as seen from |fromIntegral|,
so we use it as the continuation to be passed to
|fromIntegralC1|. This gives us |f23| which encompassrs the last two
functions in the chain.

\begin{code}
f23 :: Int -> Double
f23 li = fromIntegralC1 li f3
\end{code}

Finally we have to use |f23| as the continuation for the first
function |lengthC1|.

\begin{code}
f123 :: String -> Double
f123 s = lengthC1 s f23

\end{code}

\begin{run}
        |*Main> f123 "love"|
       \perform{f123 "love"}
\end{run}

|f123| is \emph{not} a CPS function. Is that a problem? Well, earlier
(on page \pageref{alike}) we said, that it is a good thing if the
composite and its components are of the same type. So yes, if the
composite was a CPS function, we'd certainly be happy about it. And
since we know, that passing |id| as the continuation will give us a
plain old function, we certainly won't lose much.

So, lets do it. This time we keep those |f..| functions private and
the composite is:

\begin{code}
rootlenC1 s k = let f3   d = sqrtC1 d k
                    f23  i = fromIntegralC1 i f3
                    f123   = lengthC1 s f23
                in f123
\end{code}

Note that |f3| uses |k| "from outside" as its continutaion and that
|f123| uses |s| from outside as its String argument, while the rest of
the plumbing is internal. And it works

\begin{run}
        |*Main> rootlenC1 "love" id|
       \perform{rootlenC1 "love" id}
\end{run}

Still this is not pretty and requires too much thinking. Can't we
compose two such function in a gerneric way? After all the dot
operator in the previous chapter could do such a thing. Let's write
such a combinator function and call it |andThen|.

\label{andThen1}
\begin{code}
andThen1 f1 f2 = let f12 x k = 
                             let c x2 = f2 x2 k
                             in f1 x c
                 in f12

rootlenC2 = lengthC1 `andThen1` fromIntegralC1 `andThen1` sqrtC1
\end{code}

So we take two CPS-functions |f1| and |f2| and produce a function
|f12| which takes an argument |x| and a continuation |k|. The |x|
argument is passed to the first function |f1|. The continuation |c|
for |f1| is constructed from |f2| and whatever comes after it, namely
the continuation |k| of the combined function.

\section{Geleralize}
\subsection{CPS Types}

The type of |andThen| is a bit daunting. It is:

\begin{verbatim}
andThen :: (a -> (b->r) -> r) -> (b -> (c -> r) -> r) 
        -> (a -> (c -> r) -> r)
\end{verbatim}

We can beautify this a bit by defining a new type for CPS functions,
considering the three involved type. There are several options:

\begin{code}
newtype CPS1 a b r = CPS1 (a-> (b->r) -> r)
\end{code}

Considering, that all the components in a CPS style computation work
towards the same type of final result, we may flip the type
parameters, so |r| comes first and |CPS r| becomes "a thing".

\begin{code}
newtype CPS2 r a b = CPS2 (a-> (b->r) -> r)
\end{code}

Or we may reason, that while an ordinary function |a -> b| returns a
plain vlaue |b|, a CPS function returns a function |(b->r) -> r|. So
we might define a new type just for this return value.

\begin{code}
newtype CPS3 r b  = CPS3 ((b->r) -> r)
\end{code}

Finally we can define an accessor for |(b->r) -> r|, such that we can
extract it more easily.

\begin{code}
newtype C r b  = C {runCont :: (b->r) -> r}
\end{code}

This type is very similar to an ordinary value of type |b|. In fact we
can convert an ordinary value into a |C r b| by constructing a
function which takes a continutation |k| and pass the value to the
continuation.

\begin{code}
v2c :: b -> C r b
v2c b = let f k = k b
        in C f
\end{code}

To convert a |C| value back to an ordinary value, we can pass id as
the continuation.

\begin{code}
c2v :: C r r -> r
c2v cv = runCont cv id  
\end{code}

So |c2v| and |v2c| are inverses of each other:

\begin{run}
        |*Main> (c2v . v2c) 42|
       \perform{(c2v . v2c) 42}
\end{run}

\subsection{Composing with the new type}
With this, the type of |andThen| could be written as

\begin{code}
andThenC :: (a -> C r b) -> (b -> C r c) -> (a -> C r c)
\end{code}

To define this combinator, we take the earlier definition of
|andThen1| from page~\pageref{andThen1}. Where we originally just
applied one of the the functions to an argument as in |f1 x| we must
now write |runCont (f1 x)|. 

\begin{tabular}{l l l}
function type f & call & result type \\
\hline
\texfamily a~\char'31~b & \texfamily f x & \texfamily b\\
\texfamily a~\char'31~(b~\char'31~r)~\char'31~r & \texfamily f x & \texfamily (b~\char'31~r)~\char'31~r\\
\texfamily a~\char'31~(C r b) & \texfamily runCont(f x) & \texfamily (b~\char'31~r)~\char'31~r\\
\hline
\end{tabular}

\medskip This gives us the desired function |f12|, But |f12| is still
as before a function |a -> (c -> r) -> r| and not |a -> C r c|. But we
cannot just wrap the whole thing into a |C|, we only want the result
to be wrapped.

To convert a |f1 :: a -> (c -> r) -> r| into a | fc :: a -> C r c|, we
need to construct a function |fc| which takes an |a| and passes it to
|f1|. The result is a |(c -> r) -> r| and we need to wrap in in a |C|.88

\begin{code}
toC :: (a -> (c -> r) -> r) -> (a -> C r c)
toC f1 = let fc a = C (f1 a)
         in fc
\end{code}

Or more concisely, we can just write:
\begin{code}
toC' :: (a -> (c -> r) -> r) -> (a -> C r c)
toC' f1 = \a -> C (f1 a)
\end{code}

So,

\begin{code}
andThenC f1 f2 = let f12 x k = 
                             let c x2 = runCont(f2 x2) k
                             in runCont(f1 x) c
                 in \a -> C (f12 a)
\end{code}

Altertively, we can write a function

\begin{code}
-- bindC :: C r b -> (b -> C r c) -> C r c
\end{code}

You may justify this, by saying that applying the first function to an
argument (as we did before) is not the interesting part, but all the
magic is in the result. Where an ordinary function would return a
plain |b| we now have to deal with a |C r b| result.

\begin{code}
-- bindC crb f2 = let crb k = 
--                              let c x2 = runCont(f2 x2) k
--                              in runCont crb c
--                  in C crb
\end{code}

except we now must write everything in terms of this new type
|C|. Using |v2c| this is easy. Instead of returning the result right
away, we pass it to |v2c|.


\begin{code}
sqrtCPS :: Double -> C r Double
sqrtCPS         = v2c . sqrt

fromIntegralCPS :: Int -> C r Double
fromIntegralCPS = v2c . fromIntegral

lengthCPS :: String -> C r Int
lengthCPS       = v2c . length 
\end{code}

To write the combinator |andThen|, we need to construct a function |f12|

\begin{code}
andThen :: (a -> C r b) -> (b -> C r c) -> (a -> C r c)
andThen f1 f2 a = C $ \k -> let g2 x = runCont (f2 x) k
                            in runCont (f1 a) g2


            

foo = do
    x <- getLine
    return x


\end{code}


\section{Concurrency}

Continuations are useful















%\begin{figure}[htb!]
%\centering
%\includegraphics[width=4cm]{glass-slipper.jpg}
%\end{figure}

\end{document}
