\section{Simulation}

\subsection {Library code}

The following section describes the groundwork for the simulation
program. Feel free to skip this section.

\subsubsection{Helpers}
\begin{code}
import System.Random
import Data.List
import Data.Tuple
import Data.HashTable
import Text.Printf
import Debug.Trace
\end{code}

We define a Mailpiece \verb+Mpc+ as a Mail Class plus a Sortcode. We
use \verb+Mpcs+ for a set of Mailpieces.

\begin{code}
type Class = Char
type Sortcode = Int
type Mpc = (Class, Sortcode)
type Mpcs = [Mpc]
\end{code}

We give names to certain sets of mailpieces for better readability

\begin{code}
type Todays = Mpcs
type Good = Mpcs
type Bad = Mpcs
type Stored = Mpcs
\end{code}

In the end, we want to test algorithms which separate \verb+Good+ mail
from \verb+Bad+ mail. We call such an algorithm a \verb+Judge+. A
Judge can see all mail which was processed in the past, including
today's \verb+Good+ abd \verb+Bad+ mail plus all the \verb+Stored+
mail. A \verb+Judge+ looks at a single \verb+Mpc+ and answers whether
or not it is \verb+Good+.

\begin{code}
type Judge = Good -> Bad -> Stored -> Mpc -> Bool
\end{code}

To test the quality of a Judge we will need to count the number of
single-letter deliveries in a set of mailpieces.

\begin{code}

-- helper to merge to equally sized (or infinite) lists
merge xs ys = concatMap (\(x,y) -> [x,y]) (zip xs ys)

-- Helper to compare two mailpieces by Sortcode
bySortcode :: Mpc -> Mpc -> Ordering
bySortcode (c1,s1) (c2,s2) = compare s1 s2

-- Helper predicate for grouping
hasSameSortcode m1 m2 = (bySortcode m1 m2 == EQ)

countSingles :: Mpcs -> Int
countSingles mpcs = length $ filter isSingle $ groups
        where
            -- groups of mpcs sharing the same sortcode
            groups = groupBy hasSameSortcode (sortBy bySortcode mpcs)
            isSingle ms = (length ms == 1)

-- String representation of the percentage of single-letter deliveries
singlePercent :: Mpcs -> String
singlePercent mpcs  = printf "%.2f%%" (ratio::Float)
        where
            -- address is the total number of addresses, a global variable
            ratio = fromIntegral(100*singles) / fromIntegral(addresses)
            singles = countSingles mpcs
\end{code}

\subsubsection{Generating Mail}

We need a way to generate random mailpieces with a known
distribution over Class-A and Class-B and a known $\lambda$.

\begin{code}
 
randomMpcs :: Int -> Int -> Int -> Mpcs
randomMpcs seed count max = map mkMpc randomInts 
        where 
            mkMpc i = (mailClass i, i)
            randomInts = take count . randomRs (0, max) . mkStdGen $ seed
            mailClass x
                    | (mod (hash x) 4 == 0) = 'B'
                    | otherwise = 'A'
            hash x = hashString (show x)
\end{code}

Now we produce a series of mailpieces for each day. The result is an infinit List of Lists. 

\begin{code}
mpcsPerDay = 10000
addresses = 6 * mpcsPerDay

mkMpcs :: Int -> [Mpcs]
mkMpcs seed = todays : mkMpcs nextSeed
        where
            todays = randomMpcs seed mpcsPerDay addresses
            nextSeed = snd $ head todays
\end{code}

\subsubsection{Processing of  Mail}

The processing of a single day is done by the \verb+sortTodays+
function. It processes today's mail together with mailpieces which
were taken from store. The \verb+Judge+ is able to see the mailpieces
from store and the ones, which are already processed today, but not
today's mailpieces which are yet to be processed.


\begin{code}
sortTodays :: Judge -> Todays -> Stored -> (Good, Bad) 
sortTodays judge todaysMpcs fromStore = (g, b) 
        where 
            -- add \verb+fromStore+ to \verb+todaysMpcs+ and start with empty \verb+good+ and \verb+bad+
            (g,b) = sortMpcs (todaysMpcs++fromStore) [] [] 

            sortMpcs :: Mpcs -> Good -> Bad -> (Good, Bad)
            -- mailpieces are exhausted:
            sortMpcs [] good bad = (good, bad) 
            -- add mailpiece to either good or bad, depending on what the judge sais:
            sortMpcs (m:ms) good bad 
                    | judge good bad fromStore m = sortMpcs ms (m:good) bad 
                    | otherwise = sortMpcs ms good (m:bad) 
\end{code}

To process mail for a number of days, we need "today's" mailpieces for
each day and a judge for each day. We process all of the stored
mailpieces and put the bad mailpieces back into storage for tomorrow's
processing. As we stroll along we accumulate the number of good
mailpieces produced each day.

\begin{code}
sortAll :: [Judge] -> [Mpcs] -> Stored -> [Good] -> [Good]
sortAll _ [] _ good = good -- no more days to process
sortAll (j:js) (m:ms)  stored  good = sortAll js ms newStore newGood
        where
            (g,b) = sortTodays j m stored
            newStore = trace ("bads: " ++ (show $ length b)) b
            newGood = g:good
\end{code}


To run a simulation over seven days we use
\begin{code}
simulate judges = map singlePercent $ sortAll judges  mail [] [] 
        where
            mail = (take days $ mkMpcs 1)
            days = 7

\end{code}

\subsection{Judges} 

The ultimate goal of what we're doing here is to find a clever way to
distinguish good mail from bad mail. The simuation code encapsulates
this decision into the concept of a \verb+Judge+. Our work is to
define a series of Judges (one for every day) which outperforms XY.

\subsubsection{The Gullible Judge} 

The simplemost judge is one which just accepts everything

\begin{code}
gullibleJudges::[Judge]
gullibleJudges  = repeat j
        where
            j _ _ _ _ = True
\end{code}

We would expect that the \verb+gullibleJudge+ leads to 14.1\%
single-letter deliveries, the value we computed earlier, using the
Poisson-distribution. Let's try:

\begin{code}
testGullible = simulate gullibleJudges
\end{code}

With 10,000 mailpieces each day, this returns

\verb+["14.07%","14.25%","14.19%","14.14%","14.14%","14.14%","14.08%"]+

which is  in line with our expectations.

\subsubsection{The XY Judge} 

The XY Judge accepts Class-A mail on every day, but only half of the Class-B
mail. There are two different Judges, one for odd days and one for
even days.

\begin{code}
xyJudge :: Int -> Judge
xyJudge n _ _ _ (c,s) 
        | c == 'A' = True
        | mod s 2 == n = True
        | otherwise = False


xyJudges :: [Judge]
xyJudges = merge (repeat $ xyJudge 0) (repeat $ xyJudge 1)
\end{code}

We would expect this to be slightly better than the gullibleJudge. Let's try

\begin{code}
testXY = simulate xyJudges 
\end{code}

This returns

\verb+["13.59%","13.62%","13.68%","13.61%","13.52%","13.59%","12.26%"]+

These numbers are slightly better than the expected 13.97\%. I don't
have an explanation for this.


\subsubsection{myJudge}

My Judge operates as follows:
\begin{itemize}

\item If the mpcs has a sortcode which is known to be in store, accept
      it. Otherwise we may end up accumulting more and more mailpieces
      in the store, which would give us apparently fantastically low
      single-letter deliveries.

\item Otherwise, if we processed a \emph{significant amount} of
      mailpieces, then only
      accept the mailpiece if another mailpiece with the same sortcode was
      seen previously.

\item Otherwise resort to the XYJudge

\end{itemize}

\begin{code}
containsCode :: Mpcs -> Mpc -> Bool
containsCode mpcs (cl,sc) = ((find hasThisCode mpcs) /= Nothing)
        where
            hasThisCode (c,s) = (s == sc)
 

myJudge :: Int -> Judge
myJudge n good bad store (c,s) 
        | containsCode store (c,s) 
                = True
        | 100 * (length good + length bad) `div` mpcsPerDay >= 30 
                = containsCode good (c,s)
        | otherwise 
                = xyJudge n good bad store (c,s) 


myJudges = merge (repeat $ myJudge 0) (repeat $ myJudge 1)
testMy = simulate myJudges 
\end{code}

With these settings we get:

\verb+["11.30%","11.28%","11.37%","11.22%","11.20%","12.03%","3.62%"]+

\section{Conclusions}

My gut feeling, that a self-learning algorithm can reduce
single-letter deliveries without requiring any pre-knowledge, has not
been disproved so far and thus still looks promising.

Comparing the three algorithms over seven days by their percentages of
single-letter deliveries yields:

\begin{verbatim}
"testGullible"
bads: 0
bads: 0
bads: 0
bads: 0
bads: 0
bads: 0
["14.07%","14.25%","14.19%","14.14%","14.14%","14.14%","14.08%"]

"testXY"
bads: 1269
bads: 1253
bads: 1233
bads: 1263
bads: 1238
bads: 1255
["13.59%","13.62%","13.68%","13.61%","13.52%","13.59%","12.26%"]

"testMy"
bads: 7072
bads: 6287
bads: 6322
bads: 6346
bads: 6451
bads: 6351
["11.30%","11.28%","11.37%","11.22%","11.20%","12.03%","3.62%"]
\end{verbatim}

My algorithm keeps more mailpieces in store ("bads"), but no mailpiece
stays there for longer than a day. The algorithm reduces the
single-day deliveries by two percentage points, which is a massive
improvement over XY.


\begin{code}
main = do
    print "testGullible"
    print testGullible
    print "testXY"
    print testXY
    print "testMy"
    print testMy
\end{code}
