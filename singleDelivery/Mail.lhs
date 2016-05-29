%include lhs2TeX.fmt
%include colorcode.fmt
%include greek.fmt
\renewcommand{\hscodestyle}{\small}

\subsection{Mail}

This section contains thoughts which concern the generation of random
mail. It is required to \emph{test} the main ideas of this paper but
is not essential for \emph{understanding} them.

\subsubsection{Used libraries}
\begin{code}
{-# LANGUAGE BangPatterns #-}
module Mail where
        
import System.Random
import Data.List
import Data.Tuple
-- import Data.HashTable
-- import qualified Data.HashMap as Map
import qualified Data.Map as Map
import Text.Printf
import Debug.Trace
\end{code}

\subsubsection{Mailpieces}

We define a Mailpiece \verb+Mpc+ as a Mail Class plus a Sortcode. 

\begin{code}
type Class     = Char
type Sortcode  = Int
data Mpc       = Mpc {mc ::Class, sc :: Sortcode, mday:: Int} deriving (Eq, Show)

\end{code}

\subsubsection{Mail}

Mail is a collection of Mailpieces. Since we need to test for
membership frequently we will not use a simple List, but an
additional, redundant hash. The hash keys shall be Sortcodes and its
values shall be the number of Mailpieces for this Sortcode.

\begin{code}
type MailList  = [Mpc]
type MailHash  = Map.Map Sortcode Int

data Mail = Mail (MailHash, MailList) deriving (Eq, Show)

-- Accessing Hash and List

mh :: Mail -> MailHash
mh (Mail (h, l)) = h 

ml :: Mail -> MailList
ml (Mail (h, l)) = l 

-- Create Mail with no mailpieces in it.
emptyMail :: Mail
emptyMail = Mail (Map.empty, [])

-- construct Mail from a List of Mpcs
asMail :: [Mpc] -> Mail
asMail mpcs = foldr pushMpc emptyMail mpcs

-- Add a Mailpiece
pushMpc :: Mpc -> Mail -> Mail
pushMpc  mpc mail = Mail (map', mpc:(ml mail))
        where
            map'    = Map.insert (sc mpc) (count+1) (mh mail)
            count   = countMatches mail mpc



-- remove a Mailpiece
popMpc :: Mail -> (Maybe Mpc, Mail)
popMpc (Mail(map, [])) = (Nothing, Mail (map,[]))
popMpc  mail = (Just mpc, Mail (map', tail $ ml mail))
        where
            mpc    = head (ml mail)
            count  = countMatches mail mpc
            map'  | (count == 0)  = Map.delete (sc mpc) (mh mail)
                  | otherwise     = Map.insert (sc mpc) (count-1) (mh mail)

-- Merge a list of Mail
mergeMail :: [Mail] -> Mail
mergeMail ms = asMail (concat (map ml ms))

\end{code}


\subsubsection{Queries}
\begin{code}

-- answer the maximum Sortcode
maxSc :: Mail -> Int
maxSc mail = maximum $ Map.keys $ mh mail

-- Answer the number of Mailpieces
countMpcs :: Num b => Mail -> b
countMpcs mail = fromIntegral $ Map.fold (+) 0 $ mh mail

-- Occurrences of Mailpieces with the same Sortcode as mpc
countMatches :: Mail -> Mpc -> Int
countMatches mail mpc = Map.findWithDefault 0 (sc mpc) (mh mail)

-- Count addresses receiving only a single letter
countSingles :: Num b => Mail -> b
countSingles mail = fromIntegral $ length $ filter (==1) $ Map.elems $ mh mail

-- Count the number of addresses receiving mail
countAddresses :: Num b => Mail-> b
countAddresses mail = fromIntegral $ Map.size $ mh mail

-- Answer singles per delivered-to address
countSinglesQ :: Mail -> Double
countSinglesQ mail = (countSingles mail) / (countAddresses mail)

-- Answer the Density
countDensity :: Mail -> Double
countDensity mail = (countMpcs mail) / (countAddresses mail)

-- answer whether there is a mailpiece with the same sortcode as mpc
containsSc :: Mail -> Mpc -> Bool
containsSc mail mpc = Map.member (sc mpc) (mh mail)


-- count the Mailpieces, where $lo \le sc < hi$
countMpcsWithin :: Mail -> Int -> Int -> Int
countMpcsWithin mail lo hi = Map.foldWithKey inRange 0 (mh mail)
        where
            inRange k v s
                    | k >= lo && k < hi = s + v
                    | otherwise = s


\end{code}

\subsubsection{Filtering and Splitting}

\begin{code}
filterMail :: (Mpc->Bool) -> Mail -> Mail
filterMail p m = asMail $ filter p (ml m)

-- split mail into two parts, where the first satisfies the predicate
-- and the second does not
splitMailBy :: (Mpc->Bool) -> Mail -> (Mail, Mail)
splitMailBy p m = (filterMail p m, filterMail (not.p) m)


-- split mail into two parts, such that a certain percentage
-- goes into the first part and the remainder into the second
splitMailAt :: Double -> Mail -> (Mail, Mail)
splitMailAt pct m = (cut take, cut drop)
            where
                cut f  = asMail $ f n (ml m)
                n      = floor $ (fromIntegral $ countMpcs m) * pct
\end{code}

\subsubsection{Generating Random Mail}

We do not really know how mail is distributed over addresses
(Sortcodes), but we do know, that certain addresses
\emph{consistently} get more mail than others. We would like to have a
parameter, which allows a smooth transition between an even
distribution and a skewed distribution.

To generate random Sorcodes we use a method called \emph{Inverse
transform sampling}. In short this means generating a uniformly
distributed random number between 0 and 1 and use it as the $P$ value
in the cumulative distribution $CDF(sc)$ which describes the probability
for a Mailpiece to have a Sortcode less that $sc$. The corresponding
$sc$ is then a random sample. To do this we must know the inverse
function of $CDF(sc)$.

For an even distribution we have
\begin{eqnarray}
CDF_{even}(sc) &=& \frac{sc}{SC_{max}} \\
sc           &=& CDF_{even} \cdot SC_{max} \text{\quad(inverse)}
\end{eqnarray}

\begin{figure}[htb!]
\centering
\begin{tikzpicture}[domain=0:1, scale=4]
\draw[very thin,color=gray] (0,0) grid (1,1);
\draw[thick, color=blue] plot[smooth] (\x,sqrt \x);
\draw[thick, color=black] plot[smooth] (\x,\x) node[right]{$CDF$};
\draw[color=green] plot[smooth] (\x,\x^2);
\draw[thick, color=green] plot[smooth] (\x,\x^4);

\node [color=blue] at (0.21, 0.65){$\alpha=\frac 1 2$};
\node [color=black] at (0.5, 0.5){$\alpha=1$};
\node [color=green] at (0.80, 0.2){$\alpha=4$};

\draw (0,0) -- (0,-0.1) node[below] {0};
\draw (0,0) -- (0,-0.1) node[below] {0};
\draw (1,0) -- (1,-0.1) node[below] {$SC_{max}$};

\end{tikzpicture}
\caption{Evenly distributed and skewed data}
\end{figure}

Adding some skew is as easy as that\footnote{I had some trouble
finding a function which adds adjustable skew and which still has an
inverse which can be written as a formula. So I posted this question
on Stackoverflow.Mathematics and a certain "Did" gave me the right
answer within hours. See
http://math.stackexchange.com/questions/442734/need-an-easy-cdf-for-inverse-transform-sampling}
\begin{eqnarray}
CDF_\alpha(sc) &=& \left(\frac{sc}{SC_{max}}\right)^\alpha \\
sc           &=& \sqrt[\alpha]{CDF_\alpha} \cdot SC_{max} \label{icdf}
\end{eqnarray}
Where $\alpha$ controls the skew
\begin{eqnarray}
\alpha < 1 &\leftrightarrow& \text{favours lower Sortcodes} \\
\alpha = 1 &\leftrightarrow& \text{no skew} \\
\alpha > 1 &\leftrightarrow& \text{favours higher Sortcodes}
\end{eqnarray}


In general we know how many letters an address receives per day
($\lambda$). This number is typically smaller than one, e.g. $\frac 1 6$ if
an average address receives one letter per week and a week has six
working days.

We also know the total mail volume per day ($mvpd$). From these two we
can conclude the number of Sortcodes, and if the smallest Sortcode is
$0$, then the largest is:

\begin{code}
scMax :: Double -> Int -> Sortcode
scMax lambda mvpd  = floor (fromIntegral mvpd / lambda)
\end{code}

For our random numbers we need an inverse cumulative distibution
function (ICDF) and way to produce one from $\lambda$ and $mvpd$ (See
equation (\ref{icdf})).

\begin{code}
type ICDF = Double -> Sortcode

-- Create an ICDF 
mkIcdf :: Double -> Double -> Int -> ICDF
mkIcdf alpha lambda scmax x 
        = floor $ (fromIntegral scmax) * (x ** (1/alpha))
\end{code}

We can now generate a list of Sortcodes with a known distribution $\alpha$

\begin{code}
dailySortcodes :: Int -> ICDF -> [Sortcode]
dailySortcodes seed icdf =  map icdf rs
        where
            rs    = randoms (mkStdGen seed)::[Double]
\end{code}

For the classes we assume 75\% Class-A mail

\begin{code}
dailyClasses :: Int -> Double -> [Class]
dailyClasses seed lambda = map (r2c . floor . (100*)) rs
        where
            r2c x 
                | x<75       = 'A'
                | otherwise  = 'B'
            rs =  randoms (mkStdGen seed)::[Double]

\end{code}

When we zip the two together we can generate a random set of dailyMail
and an infinite list of these for consecutive days.

\begin{code}

fromMcSc :: [Class]->[Sortcode]->Int->[Mpc]
fromMcSc [] _ _ = []
fromMcSc (mc:mcs) (sc:scs) day = (Mpc {mc = mc, sc=sc, mday=day}):(fromMcSc mcs scs day)

dailyMail seed alpha lambda mvpd day = mail
        where 
            mail   = asMail $ take mvpd list
            list   = fromMcSc c s day
            c      = dailyClasses seed lambda 
            s      = dailySortcodes (seed+1) icdf
            scmax  = scMax lambda mvpd
            icdf  = mkIcdf alpha lambda scmax


-- an infinite list with one random dailyMail for each day
mail seed alpha lambda mvpd day
        = dailyMail seed alpha lambda mvpd day : mail (seed+1) alpha lambda mvpd (day+1)

\end{code}









