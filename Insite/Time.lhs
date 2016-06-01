%include lhs2TeX.fmt
%include greek.fmt
%options ghci -fglasgow-exts

\subsection{Time}

We need a clear concept about Time. This module implements some very
basic constructs and gives us the |Timed| constructor to create
arbitrary things, which are associated with an |Instant|


\begin{code}
module Time where
-- * Time

-- | A point in time
type Instant = Double

-- | Difference between two 'Instant's
type Interval = Double

-- | Infinitly long 'Interval' or distant future
inf :: Double
inf = 1/0 

-- | Something which is associated with an 'Instant'
type Timed a = (Instant, a)
\end{code}
