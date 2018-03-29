\documentclass{article}
%include lhs2TeX.fmt
%include lhs2TeX.sty
%format <- = "\char''30"
\usepackage{graphicx}
\usepackage{float}
\usepackage[parfill]{parskip}
\usepackage{needspace}

\usepackage{xcolor}
\definecolor{shadecolor}{RGB}{200,200,200}
\newcommand{\mybox}[1]{\par\noindent\colorbox{shadecolor}{\parbox{1cm}{#1}}}

%options ghci
\usepackage[framemethod=tikz]{mdframed}
\newmdenv[frametitle=Running it,backgroundcolor=gray!05,roundcorner=2pt]{run}
%\usepackage{ucs} 
%\usepackage[utf8]{inputenc}

\usepackage{amssymb,amsmath}
%include lhs2TeX.fmt
%include lhs2TeX.sty

\widowpenalty=3000
\clubpenalty=3000

\author{Martin Drautzburg}

\title{Concurrency and Continuations for Dummies}

\begin{document} \maketitle \tableofcontents 

\begin{abstract} 
\end{abstract}
\section{Introduction}

\subsection{What do we mean by concurrency?}

The concurrency we're talking about here is running several
\emph{effectful} computations (\emph{actions}) in parallel. One such
effect could be doing IO. Here concurrency is not a means of improving
performance, but a way to improve program structure and clarity.

The other type of concurency is actually \emph{parallelism}. This is
all about spreading the load of a computation over several CPU cores
in oder to improve performance. A parallel progam always produces the
same result as a sequential program (when written properly).

\needspace{12em}
What we're really after is a way to obtain a list of effectful
computations as in:

\begin{code}
ex1 = [a1, a2, a3]
        where
            a1 = putStrLn "hello"
            a2 = do 
                putStr "enter something: "
                l <- getLine
                putStrLn $ "you entered: " ++ l
          
            a3 = putStrLn "goodbye"
\end{code}

We can \emph{run} this list of actions with a function like
this\footnote{There is a pre-defined function |sequence_| which does
  about the same thing}:

\begin{code}
exRun [] = return ()
exRun (a:as) = do
    a
    exRun as
\end{code}

This produces the output
\begin{verbatim}
        *Main> exRun ex1
        hello
        enter something: asd
        you entered: asd
        goodbye
\end{verbatim}

For real programs it is not feasible to have the programmer construct
this list of actions. We want an abstraction like Threads, where the
software computes and maintains such a list \emph{as we go}.

\section{Introduction to Continuations}

\subsection{Intuition}

Continuations tend to be hard to understand. So here is a little
example: consider you have an RPN calculator and you want to compute
the expression $\sqrt{3^2 + 4^2}$. Each step in the calculation will
manipulate the Stack. (i.e. the \emph{effect} is on the Stack).

\begin{tabular} {llr}
Operation & Stack \\
\hline
3       & [3]           \\
$x^2$   & [9]           \\
4       & \textbf{[4,9]}     & Suspended Computation    \\
\hline\hline
$x^2$   & [16,9]    & Continuation, Callback    \\
$+$     & [25]          \\
$\sqrt{}$ & [5]
\end{tabular}

I split the entire computation into an upper and a lower part. It is
easy to see, that I could pause the computation between $4$ and $x^2$
and continue later, as long as I have a way to restore the stack. In
fact I could do this anywhere, not just where I happened to draw the
double line.

The result of the upper part is (in this case) a Stack. It is not a
\emph{result}, but something on the way to become a result. Only when
we feed the Stack into the lower part, we get our final result.

Functional programmers call the lower part a \emph{Continuation} or a
\emph{Callback}. The upper part doesn't seem to have a
well-established name, but calling it a \emph{suspended computation}
is pretty common.

With this terminology we can say: 
\begin{quotation}
A suspended computation plus a continuation produces a result.
\end{quotation}

Note that the above explanations are in no way tied to RPN
calcualators manipulating a Stack. This is just an example which is
easy to understand. Also there is no reason to believe that RPN
notation is particularly powerful.

\subsection{Concurrency}

The \emph{Continuation} metaphor is helpful for implementing
concurrency. This is because it introduces something like a
\emph{predetermined breaking point}. A scheduler could do something
else when the execution reaches the double line and resume computation
later on. In the end it will always execute a list of actions, but
that list no longer has to be written by the programmer, but is the
result of interrupting and resuming computations. Particularly that
list can grow and shrink as the execution progresses.

The building blocks of our concurrency implementations are thus things
which 
\begin{itemize}
\item do something effectful and then 
\item return what to do next.
\end{itemize}\label{items:conc}

These two parts correspond to \emph{Suspended Computation} and
\emph{Continuation}.

\subsection{IO Operations}

Let's abandon the Stack and turn our attention to IO operations. Such
operations do not manipualte a Stack, but ``the World''. 

Remember that in Haskell, the type |IO a| stands for something which
does IO and returns an |a| to the program. Hence |IO ()| is an
operation which returns nothing usable, but still does IO, such as
\emph{output} operations like |putStrLn|. |IO String| in contrast does
IO but also returns a |String| to the program, which is typical for
\emph{input} operations.

What we need here is an operation which does IO and returns what to do
next.

\begin{code}
data SuspComp1 = SuspComp1 (IO SuspComp1)
\end{code}

However, such suspended computations would have no way to
terminate. They are obliged to always return yet another suspended
computation. To cicumvent this, we allow it to alternatively return an
indication that it wants to terminate. This is a thing, which can run
as long as it returns continuations and then eventually stops. We call
it a |Thread|.

\begin{code}
data Thread2 = Atom2 (IO Thread2) | Stop2
\end{code}

If we have a list of such |Thread2| thingies we can run it in the
following way:

\begin{code}
run1 [] = return ()
run1 (a:as) = case a of
                  Atom2 a  -> do 
                         a' <- a  -- run the head of the list 
                                  -- and grab the continuation
                         run1 (as ++ [a']) -- put continuation at 
                                           -- the end of the list,
                                           -- run the new list
                  Stop2 -> do
                         putStrLn "Stopped." 
                         run1 as -- run only the tail of the list
\end{code}

Everytime an IO action is performed its returned continuation
is appended to the list, while the head of the list is discarded.

Here is a function which constructs a |Thread2|. It takes a String and
a Counter |i|, produces some output and then returns itself with a
decremented counter as the continuation. When the counter reaches
zero, it returns a |Stop2| as the continuation.

\needspace{5em}
\begin{code}
exThread1 name i = Atom2 $ do
                       putStrLn $ name ++ " " ++ (show i) ++ ","
                       return $ case i of
                                  0 -> Stop2
                                  _ -> (exThread1 name (i-1))
\end{code}

\needspace{12em}
\begin{run}
|*Main> run1 [exThread1 "foo" 3, exThread1 "bar" 1]|\\
  \perform{run1 [exThread1 "foo" 3, exThread1 "bar" 1]}
\end{run}

So our |Thread1| type in conjunction with |run1| really produces a
list of interleaved IO actions, and the list is manipulated \emph{as
we go}. 

\subsection{Step-by-step Threads}

This works nicely, but only because our Threads were extraordinarily
simple. They were essentially loops, written as recursive
functions. What if we want a Thread which does something, then
something else and then some more? The problem lies in the fact that
after each step the Thread needs to return its own continuation. This
is doable, but it's not pretty:

\begin{code}
exThread2 = Atom2 $ do
                putStrLn "step 1,"
                return $ Atom2 $ do
                        putStrLn "step 2,"
                        return $ Atom2 $ do
                             putStrLn "step 3,"
                             return Stop2
\end{code}

The code tends to drift to the right. But it works

\needspace{12em}
\begin{run}
|*Main> run1 [exThread2, exThread1 "foo" 1]|\\
  \eval{run1 [exThread2, exThread1 "foo" 1]}
\end{run}

The code looks ugly, because there is no special support for chaining
|Thread2| functions. Hence we had to resort to \$ for
chaining. Without \$ we would have had to use parentheses, which looks
even uglier.
\needspace{10em}
\begin{code}
exThread3 = Atom2 ( do
                putStrLn "step 1,"
                return ( Atom2 ( do
                        putStrLn "step 2,"
                        return ( Atom2 ( do
                             putStrLn "step 3,"
                             return Stop2)))))
\end{code}

\subsection{Chaining}

When we look at the above code, we can see an element of repetition,
namely the items we listed on page \pageref{items:conc}: \emph{do
something and then return a coninuation}.

We can write a function which does just that:

\begin{code}
atom2 :: IO a -> Thread2 -> Thread2
atom2 ioOperation continuation = 
        Atom2 $ do ioOperation
                   return continuation
\end{code}

Then we would like to write something like:

\begin{code}
exThread4 :: Thread2 -> Thread2
exThread4= (atom2 (putStrLn "step 1,"))
           `chainWith`
           (atom2 (putStrLn "step 2,"))
           `chainWith`
           (atom2 (putStrLn "step 3,"))
\end{code}

We need to omit the final |Stop2|, because that would turn the whole
thing into a |Thread2|, instead of the open-ended |Thread2->Thread2|
type. It turns out the |chainWith| function is simply function
composition:

\begin{code}
chainWith :: (Thread2 -> Thread2) -> (Thread2 -> Thread2) 
          -> (Thread2 -> Thread2)
chainWith' a1 a2 = a1 . a2 -- long version
chainWith = (.) -- short (point-free) version
\end{code}

So we can just write:
\begin{code}
exThread5 :: Thread2 -> Thread2
exThread5= (atom2 (putStrLn "step 1,")) .
           (atom2 (putStrLn "step 2,")) .
           (atom2 (putStrLn "step 3,"))
\end{code}

Now when we run the whole thing, we must turn the function |Thread2 ->
Thread2| into a |Thread2| by passing it a final operation, namley
|Stop2|. And we can of course still start multiple threads.

\begin{run}
|*Main> run1[exThread5 Stop2, exThread1 "foo" 1]|\\
  \eval{run1[exThread5 Stop2, exThread1 "foo" 1]}
\end{run}

\section{Real Continuations}
\subsection{Local State}

Or examples are sill somewhat simplistic, because each |Atom2| is
completely isolated from the other.  One |Atom2| cannot pass a value
to the next. In fact, this is a consequence of our chaining function,
because the nested |do| blocks are well capable of doing that.

\begin{code}
exThread7 x = Atom2 $ do
                let x1 = x+5 -- $\leftarrow$ \mybox{here}
                putStrLn $ "x1=" ++ (show x1)
                return $ Atom2 $ do
                          let x2 = x1+10 -- $\leftarrow$ \mybox{here}
                          putStrLn $ "x2=" ++ (show x2)
                          return $ Atom2 $ do
                                     let x3 = x1+x2 -- $\leftarrow$ \mybox{here}
                                     putStrLn $ "x3=" ++ (show x3)
                                     return Stop2
\end{code}

\begin{run}
|*Main> run1 [exThread7 11]|\\
  \eval{run1 [exThread7 11]}
\end{run}

\subsection{State as the root of all evil}

Effectful computations in Haskell can be seen as operating in two
areas. One is the traditional functional world, where a function does
nothing but return a value. The other is something implicit and
hidden. In imperative languages this is the accessible \emph{State}.

It might be worth while discussing what the accessible State in an
object-oriented progam is. If you imagine yourself to be within a
method, what State do you have access to?

\begin{itemize}
\item the local variables defined in the method
\item the member variables of your object
\item the member variables of your object's class (i.e. the class variables)
\end{itemize}

Now each of these variables can hold other objects on which you can
invoke methods. So implicitly you have the power to change the State
of all referenced objects which expose state-changing methods. And
through these you can change the State of \emph{their} referenced
objects. This is big.

A consequence of this is that you will be unable to guarantee that
some aspect of the state will \emph{not} be changed by the method
under consideration.

Some believe that this is the root of all evil. In fact the history of
programming languages is paved with attempts to limit access to the
State of the program in clever ways. Object-orientation is one
outcome, but any scoping rule attacks the same problem.

\subsection{Passing values}

So far we're composing function from |Thread| to |Thread|, which gives
us new functions from |Thread| to |Thread|. In the end we had to pass
a final Action, such as |Stop2| to bring the whole thing to its
conclusion.

New if we additionally want to pass a value from one |Action ->
Action| function to the next, we'll end up with 

\begin{verbatim}
Action -> (v -> Action) ->
\end{verbatim}

\end{document}


