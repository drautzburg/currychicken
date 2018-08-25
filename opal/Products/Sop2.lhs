%include lhs2TeX.fmt
%options ghci -fglasgow-exts

%if False
\begin{code}
import qualified Data.List as L
import qualified Data.Set as S
import qualified Data.Map as M
import qualified Data.Foldable as F
import qualified Data.Monoid as Mon
import qualified Data.Tuple as T
import Text.Show.Pretty


import Data.Maybe
import Data.Function
import Data.Char
import Control.Monad
import Control.Monad.Reader
import Debug.Trace
import Test.QuickCheck

pp x = putStrLn $ show x
lpp a = do
    putStrLn "\\begin{verbatim}"
    putStrLn $ ppShow a
    putStrLn "\\end{verbatim}"

ts x = trace ("*** " ++ show x ++ " ***") x
\end{code}
%endif

xxx need to sort ?

\subsection{Compression}

\subsubsection{Compressing Pairs}

The purpose of compression is to convert a collection of |(x,y)| Pairs
into a collection of Pairs, where one or both components are a
aggregated to collections.

Compression is an inverse to computing a cartesian product, which is
the corresponding \emph{uncompress} operation and much easier to
write.

With two collections, each containing $100$ elements, the cartesian
product contains $10000$ Pairs. A compress operation would undo the
cartesian product and convert this list of $10000$ pairs into two
collections of $100$ elements each.


\begin{code}
pUncompressLeft zs = [(x,y)  |
                      (x,ys) <- zs,
                      y <- ys
                     ]

pUncompressRight zs = [(x,y)  |
                       (xs,y) <- zs,
                       x <- xs
                      ]

\end{code}


The more interesting compress operations group all elements with a
common Left component and aggregate the Right
components\footnote{This is equivalent to the SQL |group by|
operation, with the Left component being the column which is grouped
over and the Right column being passed to a concatenation aggregate
function (which I believe only exists in newer versions of Oracle, and
there only for Strings}. There are similar operations which operate on
common Right elements or on both components.

\begin{code}
pCompressLeft :: (Eq a) => [(a,b)] -> [(a,[b])]
pCompressLeft [] = []
pCompressLeft ((a,b):zs) = let (bs,ys) = span ((==a).fst) zs
                          in (a, b:map snd bs) : pCompressLeft ys
\end{code}

\needspace{12em}
\begin{code}
pCompressRight [] = []
pCompressRight ((a,b):zs) = let (as,xs) = span ((==b).snd) zs
                           in (a:map fst as, b) : pCompressRight xs
\end{code}

\begin{note} 
The Left and Right |uncompress| operations commute, i.e. when you
apply one after the other, the order does not matter. This is not the
case for the |compress| operations. It can make a difference whether
you compress Left first and then Right or the other way round (see
Tests below) 
\end{note}

\subsubsection{Examples}

\begin{code}
ex_pairs1 = [(a,b) | 
             a<-['a'..'z'], 
             b<-[1..10]
            ]
\end{code}

\begin{run}
|*Main> length ex_pairs1|\\
  \eval{length ex_pairs1}

|*Main> (pCompressRight . pCompressLeft)  ex_pairs1|\\
  \eval{(pCompressRight . pCompressLeft)  ex_pairs1}
\end{run}

xxx:
\begin{code}
ex_pairs2 = S.fromList [(a,b) | 
                        a<-['a'..'h'], 
                        b<-filter odd [1.. (1 + ord a - ord 'a')]]
\end{code}

\subsubsection{Tests}

The following should always be true:

\begin{code}
-- helper to compare lists irrespective of the sequence of elements
equalsList xs ys = let normalize = L.nub.L.sort
                   in normalize xs == normalize ys

-- uncompress undoes compress (3 tests)
prop_left  xs = pUncompressLeft  (pCompressLeft xs)  `equalsList` xs

prop_right xs = pUncompressRight (pCompressRight xs) `equalsList` xs

prop_both  xs = let compBoth  = pCompressRight  . pCompressLeft
                    ucompBoth = pUncompressLeft . pUncompressRight
                in ucompBoth  (compBoth xs)  `equalsList` xs

-- uncompress commutes
prop_commute xs = (pUncompressRight . pUncompressLeft)  xs == 
                  (pUncompressLeft  . pUncompressRight) xs

\end{code}

\subsubsection{Compressing Lists}

If we use the same label-type |tly| for all our items, we can express
Products as Lists, where ach element in that list is a label and the
list as a whole describes one possible |Path| (of packings) to a deeply
nested item.

To get a |Product| we need a List of Paths, to cater for all possible
combinations. So, a Product would be a List of such Lists.

The compressed version of such a list contains elements we call
|Grids|. A Grid is a List of Lists and the cartesian product of all
these Lists gives us back part of the Product, i.e. a part that can be
written as a cartesian product. In general a Product cannot be
written as a single cartesian product, so to cater for any possible
Product, we need a List of Grids.

\begin{code}
type Path a = [a]
type Grid a = [[a]] 
\end{code}

Before we dive into compressing Products, let's first write the easier
uncompress function, which is at its heart, just a cartesian product.

\begin{code}
lUncompGrid :: (Show a) => Grid a -> [Path a]
lUncompGrid [] = [[]]
lUncompGrid (x:xs) = do
    a <- x               -- a  :: lbl
    as <- lUncompGrid xs -- as :: [lbl]
    return (a : as)
\end{code}

We really need to uncompress Lists of Grids, but that's just

\begin{code}
lUncompress gs = (concatMap lUncompGrid) gs
\end{code}

\needspace{16em}
Let's try this out. Here is a List of Grids:

\begin{code}
ex_grids1 :: [Grid Int]
ex_grids1 = [grid1, grid2]
        where
         grid1 = [ -- a 1 or 2 can contain a 3 or 4
                   [1,2],
                   [3,4]
                 ]
         grid2 = [ -- a 1 or 2 can contain a 20 which can contain a 30
                   [1,2],
                   [20],
                   [30]
                 ]
\end{code}

\begin{run}
|*Main> lUncompress ex_grids1|
  \perform{lpp $ lUncompress ex_grids1}
\end {run}


\subsubsection{Processes}

Experiments about Items and Products as Lists

\begin{code}

data Product a = Empty [a] | Nonempty ([a]->[a])                                                          

-- pack Paths into container with known label
-- ppack :: a -> [Path a] -> [Path a]
ppack clbl p = map (clbl :) p

fpack :: a -> ([a] -> [a]) -> ([a] -> [a])
fpack clbl f = (clbl:) . f 

fpackAll :: a -> [[a] -> [a]] -> [[a] -> [a]]
fpackAll clbl fs = map (fpack clbl) fs

ne as = (as ++)
ex_nonempty = [
 ne [1,2,3],
 ne [4,5,6]
 ]

ex_1 = map ($ []) ((fpackAll 10 ) ex_nonempty)
\end{code}


xxx:
\begin{code}
-- lCompressLeft :: (Eq a) => [a] -> [[a]]
-- lCompressLeft [] = []

xxy = [
 (1,[[2,3],[2,7]]),
 (2,[[3,4]])
 ]

lCompressLeft zs = let toPair (x:y:[]) = (x, [y])
                       toPair (x:xs)   = (x, xs)
                       fromPair (a, bs) = [[a]] : bs : []
                       cl xs = let ps = pCompressLeft  (map toPair xs) 
                               in map fromPair ps
                   in do
                       ss <- cl zs
--                       (b, bs) <- pCompressLeft  (cl as)
                       ss


                             

xxx = [
       [1,2,3],
       [1,2,7], 
       [2,3,4]
      ] :: [[Int]]

\end{code}



