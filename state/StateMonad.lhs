\documentclass{article}
%include lhs2TeX.fmt
%include lhs2TeX.sty
%format <-- = "\boldmath{${\leftarrow}$}"
%format <- = "\char''30"
\usepackage{fancyhdr}
\pagestyle{fancy}
\usepackage{ragged2e}
\usepackage[parfill]{parskip}
\usepackage{pgf}
\usepackage{tikz}
\usepackage[utf8]{inputenc}
\usetikzlibrary{arrows,automata}
\usetikzlibrary{positioning}
\tikzset{
    state/.style={
           rectangle,
           rounded corners,
           draw=black, very thick,
           minimum height=4em,
           minimum width=4em,
           fill=blue!10,
           inner sep=2pt,
           node distance=2cm,
           text centered,
           },
}
\author{Martin Drautzburg}

\title{The State Mondad slowly dissected}

\begin{document} \maketitle \tableofcontents 

\section{The type}
The type we are dealing with is the following:

\begin{code}
import Control.Monad.State
newtype State s a = State { runState :: s -> (a, s) }
\end{code}

To understand this type we need to understand

\begin{itemize}
\item The |newtype| and |data| keywords
\item Record syntax
\item Wrapped functions
\end{itemize}

I will not go into |newtype| and |data|, but I will explore the other
two topics.

\subsection{Record syntax}

Record syntax allows to define a tuple together with access functions
to retrieve specific components of the tuple. To understand the
motivation behind it, we will first try without record syntax.

\subsubsection{Tuples}
Consider a Pair of |Int| and |String|, where want to refer to the
first component as |foo| and to the second component as |bar|. With
such a type we can write access functions which retrieve either the
first or second component and which have a name which reflects the
name of the component.

\begin{code}

type PairTuple = (Int, String)

-- access functions:
fooTuple :: PairTuple -> Int
fooTuple (foo,_) = foo

barTuple :: PairTuple -> String
barTuple (_,bar) = bar
\end{code}

If we want to modify one of the components, we need additional
functions like

\begin{code}
modFooTuple::Int -> PairTuple -> PairTuple
modFooTuple foo (_, bar) = (foo, bar)

modBarTuple::String -> PairTuple -> PairTuple
modBarTuple bar (foo,_) = (foo, bar)
\end{code}


As an exercise let's create a Pair and then modify each of the
components and finally retrieve the bar compnent

\begin{code}
ex1 = barTuple $ (modBarTuple "changed") $ (modFooTuple 2) $ (1,"init")
\end{code}

\begin{verbatim}
*Main> ex1
"changed"
*Main> 
\end{verbatim}

\subsubsection{Records}

It seems unneccessary to define these four functions, because all the
system needs to know is the types and names of the components. This is
where record syntax comes in. We cannot get away with a simple |type|
synonym anymore, but must define a new type, e.g. using the |data|
keyword. We cannot use |newtype| because the constructor has more than
one field.

\begin{code}
data PairRecord = PR {foo::Int, bar::String} deriving (Eq,Show)
\end{code}

This definition magically creates two functions |foo| and |bar| which
correspond to the |fooTuple| and |barTuple| functions we created
ourselves in the previous example.

\begin{verbatim}
*Main> :t foo
foo :: PairRecord -> Int
*Main> :t bar
bar :: PairRecord -> String
\end{verbatim}

Using record syntax, we can create |PairRecord|s ...

\begin{code}
pr1 = PR {foo=1, bar="init"}
\end{code}

retrieve the components ...

\begin{verbatim}
*Main> foo pr1
1
*Main> bar pr1
"init"
\end{verbatim}

and even update records

\begin{verbatim}
*Main> pr1 {bar = "changed"}
PR {foo = 1, bar = "changed"}
*Main> 
\end{verbatim}

where an \emph{update} is of course not a real update, but the
construction of a new PairRecord with one or more components changed.

\subsection{A type which wraps a function}

The |State| type however, does not consist of simple types like |Int|
and |String| but wraps around a function. To get a feeling of what
this does, let's again create such a Type ourselves.

\begin{code}
data FuncRecord = FR{run :: Int -> Int}
\end{code}

To create such a record we must pass a function into the data
constructor |FR|. This function could be an already existing function, or
one which we create on-thy-fly, using a lambda expression. Let's try both:

\begin{code}

inc x = x+1
frInc = FR inc
frDouble = FR  (\x -> x*2)
\end{code}

So what we can do with such things? We know we can retrieve the |run|
component, which will give us a function. This function can then be
applied to an argument.

\begin{verbatim}
*Main> (run frInc) 5
6
*Main> (run frDouble) 5
10
\end{verbatim}

\section{Monads}

A Monad can often be seen as a \emph{something of something else}. If
you have a |List| of |Int|s, then the \emph{something} is |List| and
the \emph{something else} is |Int|.

In type signatures you often see a thing like |M a| (or |m a|). Here
|M| is the \emph{something} and |a| is the \emph{something else}. The
|M| is called a \emph{type constructor} as it creates a new type from
a base type |a|. If there was a type constructor |List|, then a List
of Ints would be written as |List Int|.

As far as monadic operations are concerned, the |Int| is of little
concern. The monadic operators like |return| and |>>=| |("bind")|are
spefic to the \emph{type constructors} only.

This kind of abstraction is very common in Haskell. E.g. the |reverse|
operation on Lists works on Lists of any types. It knows nothing about
the type of elements in the List. This is the way it should be: 

\begin{quote}
\emph{A function which reverses a list of bananas knows nothing about bananas}
\end{quote}

However, Mondads are not about reversing, but about chaining. It is a
good idea to know the type of the bind operator |>>=| by heart.

\begin{verbatim}
(>>=) :: Monad m => m a -> (a -> m b) -> m b
\end{verbatim}

This just sais, that |(>>=)| creates a new monadic value from an old
monadic value with the help of a function. It sais nothing about
\emph{how} this is done. There are in fact several options, but mostly
one of them is overwhelmingly more useful than the others.


%\path [draw, thick, ->] (f1) edge[out=0, in=-90, in distance=2cm] node[below]{a} (g.south) ;

\subsection{A first monad}

Let's try to roll our own monad from our |FuncRecord| from above. We
must change a number of things. First a monad needs a type variable
(the \emph{of something else}, i.e. the bananas). So instead of
functions from Int to Int, we use functions from Int to some type |a|.

Then we must rename a number of things, to avoid name clashes with our
|FuncRecord|

\begin{code}
data FuncRecordMonad a = FRM{runm :: Int -> a}

-- some examples:
frmInc = FRM (\x -> x+1)
frmDouble = FRM (\x -> x*2)

\end{code}

The experiments we did with the |FR| type will work with |FRM| just as
well, namely |(runm frmInc) 5| and  |(runm frmDouble) 5|

To make |FRM| a monad, we must define the two function |return| and
|(>>=)|.  

\subsubsection{Return}

The |return| function |return :: Monad m => a -> m a| takes some value
and constructs a Monad from it. In our case thus would be a function,
which returns a value of type |a|. There aren't too many options,
because the only variable whose type is definitly an |a| is the
argument to |return|, whose actual type is not known. So our only
option is to create a function, which returns this value regardless of
its input, somthing like $return\, x = FRM (\lambda \_ \rightarrow x)$

\bigskip

\begin{tikzpicture}
 \node[state, label=above:|a -> FuncRecordMonad a|] (g) {};
 \draw [thick, -*] (g.east) ++(0,0.0)   node [xshift=-5pt]  {a} -- +(0.5,0)  node (gout) {};
 \draw [thick, -*] (g.west) ++(0,-0.0)  node [xshift=8pt]   {Int} -- +(-0.5,0) node (gain) {};
 \draw [thick, -*] (g.south) ++(0,-0.0) node [yshift=5pt]   {a} -- +(0,-0.5) node (gin2) {};

 \path [draw, thick, ->] (gin2) edge[out=0, in=-90, in distance=1cm] node[below]{|x|} (gout) ;    
\end{tikzpicture}

\subsubsection{Bind}

Now we ask ourselved the question: if we have such a function, wrapped
in |FRM| and a function which creates another such such function (the
|a -> M b|), how can we construct a new |FRM| in a way which makes some
sense?

\bigskip

\begin{tikzpicture}
 \node[state, label=above:|FuncRecordMonad a|] (f) {};
 \draw [thick, -*] (f.east) ++(0, 0.0) node [xshift=-5pt]  {a} -- +(0.5,0)  node (fout) {};
 \draw [thick, -*] (f.west) ++(0,-0.0) node [xshift=8pt]   {Int} -- +(-0.5,0) node (fin) {};

 \node[state, right of=f, xshift=2cm, label=above:|a -> FuncRecordMonad b|] (g) {};
 \draw [thick, -*] (g.east) ++(0,0.0)   node [xshift=-5pt]  {b} -- +(0.5,0)  node (gout) {};
 \draw [thick, -*] (g.west) ++(0,-0.0)  node [xshift=8pt]   {Int} -- +(-0.5,0) node (gain) {};
 \draw [thick, -*] (g.south) ++(0,-0.0) node [yshift=5pt]   {a} -- +(0,-0.5) node (gin2) {};

\end{tikzpicture}



To implement |(>>=)| we must combine these into a single (wrapped)
function. There aren't too many options. Remember that

\begin{verbatim}
(>>=) :: Monad m => m a -> (a -> m b) -> m b
\end{verbatim}


The function | a -> M b| expects an |a|. We might feel tempted to pass
it some constant, but we do not really know its type. We only know it
is an "|a|". So we cannot do this. The only way we can feed it an |a|
is to take the return value from the first |FRM|.

\bigskip

\begin{tikzpicture} 
\node[state, label=above:|f:: FuncRecordMonad a|] (f) {}; 
\draw [thick,-*] (f.east) ++(0, 0.0) node [xshift=-5pt] {a}   --+(0.5,0) node (fout){}; 
\draw [thick, -*] (f.west) ++(0,-0.0) node [xshift=8pt] {Int} --+(-0.5,0) node (fin) {};

 \node[state, right of=f, xshift=3cm, label=above:|g:: a -> FuncRecordMonad b|] (g) {};
 \draw [thick, -*] (g.east) ++(0,0.0)   node [xshift=-5pt]  {b} -- +(0.5,0)  node (gout) {};
 \draw [thick, -*] (g.west) ++(0,-0.0)  node [xshift=8pt]   {Int} -- +(-0.5,0) node (gain) {};
 \draw [thick, -*] (g.south) ++(0,-0.0) node [yshift=5pt]   {a} -- +(0,-0.5) node (gin2) {};

 \path [draw, thick, ->] (fout) edge[out=0, in=-90, in distance=2cm] node[below]{|a|} (gin2) ;    
\end{tikzpicture}


We still have two |Int| inputs, but the resulting function should have
only one. We could pass a constant |Int| to the second
function. However the chosen value would be difficult to
justify. Instead we will pass the agrument to the first function also
to the second function.


\bigskip

\begin{tikzpicture} 
\node[state, label=above:|f:: FuncRecordMonad a|] (f) {}; 
\draw [thick,-*] (f.east) ++(0, 0.0) node [xshift=-5pt] {a}   --+(0.5,0) node (fout){}; 
\draw [thick, -*] (f.west) ++(0,-0.0) node [xshift=8pt] {Int} --+(-0.5,0) node (fin) {};

 \node[state, right of=f, xshift=3cm, label=above:|g:: a -> FuncRecordMonad b|] (g) {};
 \draw [thick, -*] (g.east) ++(0,0.0)   node [xshift=-5pt]  {b}   -- +(0.5,0)  node (gout) {};
 \draw [thick, -*] (g.west) ++(0,-0.0)  node [xshift=8pt]   {Int} -- +(-0.5,0) node (gin) {};
 \draw [thick, -*] (g.south) ++(0,-0.0) node [yshift=5pt]   {a}   -- +(0,-0.5) node (gin2) {};

 \path [draw, thick, ->] (fout) edge[out=0, in=-90, in distance=2cm] node[below]{|a|} (gin2) ;    
 \path [draw, thick, ->] (fin) edge[out=90, in=135, in distance=3cm] node[above]{|x|} (gin) ;    
\end{tikzpicture}

The result indeed has the type |M b|, i.e. |FuncRecordMonad b|, a
function from |Int| to |b|. To write this in proper Haskell, we first
apply |f| to |x| by means of |(runm f) x)|. This gives us some value
of type |a|. We pass this value to |g| which gives us another |FRM
b|. Finally we apply this new function to the same |x| and get a value
of type |b|. So we have constructed a new function |f2| from |f| and
|g| and the only thing left to do, is to wrap in inside |FRM|.

\begin{code}
instance Monad FuncRecordMonad where
        return x = FRM (\_ ->  x)
        f >>= g = FRM f2
                where
                    f2 x = (runm (g ((runm f) x))) x
\end{code}


\subsubsection{Chaining}

We designed our monad with no specific purpose in mind. But let's
explore what it does anyways.

|Return| alone would create a function which always returns the same
value To actually run this function we must unwrap it with |runm|.

\begin{code}
f2 = runm frm
        where
            frm = return ".org"
\end{code}

\begin{verbatim}
*Main> :t f2
f2 :: Int -> [Char]
*Main> f2 1
".org"
*Main> f2 2
".org"
\end{verbatim}

Now let's try the chaining. We must invent that second function |g ::
a -> FuncRecordMonad b|, where the b-typed value inside |FRM| is a
function from |Int| to some type |b|.


\begin{code}
f3 = runm frm
        where
            frm = return ".org" >>= \a -> FRM (\x -> (show x) ++ a) 
\end{code}

So this function converts its |Int| argument to a String and appends ".org".


\begin{verbatim}
*Main> f3 1
"1.org"
*Main> f3 99
"99.org"
\end{verbatim}

\subsubsection{Doing}

Now, let's try to rewrite |f3| using do-notation. First, let's try to
get rid of the value constructor |FRM| by using |return|. Furthermore
let's get rid of the lambda by making it an argument to |f3|. Finally
let's not unwrap right away, using |runm| but return a
|FuncRecordMonad| and leave the unwapping to the caller.

\begin{code}

f3a x = return ".org" >>= \a -> return ((show x) ++ a) 

-- This translates to do-notation
f3b x = do
    a <- (return ".org") :: FuncRecordMonad String
    return ((show x) ++ a) 
\end{code}

The type of |f3b| is now \eval{:t f3b}. If we pass it one argument,
we get \eval{:t f3b 99} from which we can extract the function
\eval{:t (runm (f3a 99))} and when we finally call this function we get:

\begin{verbatim}
*Main> (runm (f3b 99)) 666
"99.org"
\end{verbatim}

Note that the final argument 666 is actually ignored. The result only
depends on |x=99|, the argument we passed first to f3b. Let's try to
construct a more intelligent |FuncRecordMonad|, one which transforms
an existing |FuncRecordMonad|.

\begin{code}

f4 frm = do
    a <- frm :: FuncRecordMonad String
    b <- FRM $ \x -> (x + length a)
    return b
\end{code}


|f4| takes some wrapped |Int -> String| function and returns a
function, which adds the length of the String to its |Int| argument.


\begin{tikzpicture} 
\node[state, minimum width=2cm, label=above:|provided|] (f) {}; 
\draw [thick,-*] (f.east) ++(0, 0.0) node [xshift=-14pt] {String}   --+(0.5,0) node (fout){}; 
\draw [thick, -*] (f.west) ++(0,-0.0) node [xshift=8pt] {Int} --+(-0.5,0) node (fin) {};

 \node[state, right of=f, xshift=2cm, label=above:|add length|] (g) {};
 \draw [thick, -*] (g.east) ++(0,0.0)   node [xshift=-5pt]  {b}   -- +(0.5,0)  node (gout) {};
 \draw [thick, -*] (g.west) ++(0,-0.0)  node [xshift=8pt]   {Int} -- +(-0.5,0) node (gin) {};
 \draw [thick, -*] (g.south) ++(0,-0.0) node [yshift=5pt]   {String}   -- +(0,-0.5) node (gin2) {};

 \path [draw, thick, ->] (fout) edge[out=0, in=-90, in distance=2cm] node[below]{} (gin2) ;    
 \path [draw, thick, ->] (fin) edge[out=90, in=135, in distance=3cm] node[above]{|x|} (gin) ;    
\end{tikzpicture}

\begin{verbatim}
*Main>  (runm $ f4 (FRM $ \x -> show x)) 120
123
*Main>  (runm $ f4 (FRM $ \x -> take x "lkjlkjl")) 10
17
\end{verbatim}



\end{document}
