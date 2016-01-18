
The union of two |Plists| can be computed by buiding the union of the
underlying lists. Other than that, there are some obvious corner cases
concerning |Pany| and |Pnone|\footnote{to DTZ: Plist is not a Monoid, as it required Ord.}.

\begin{code}
lUnion :: Ord a => Plist a -> Plist a -> Plist a
lUnion PlAny _  = PlAny
lUnion _ PlAny  = PlAny
lUnion (Plist as) (Plist bs) = Plist (L.union as bs)
\end{code}


\subsubsection{Intersection}

Two |pNests| can be intersected, which may or may not produce a
result. Disjoint |pNests| will produce Nothing. The operation calls
|lIntersection| to intersect the possible contained items.

\begin{code}
nIntersection :: (Ord a) => Pnest a -> Pnest a -> Maybe (Pnest a)
nIntersection (Pnest a as) (Pnest b bs)
  | a == b    = Just $ Pnest a (lIntersection as bs) 
  | otherwise = Nothing
\end{code}
\medskip

\begin{run}
Intersecting disjoint

|*Main> nIntersection ex_foo ex_bar|\\
  \eval{nIntersection ex_foo ex_bar}

Intersection with oneself

|*Main> nIntersection ex_bar ex_bar|\\
  \eval{nIntersection ex_bar ex_bar}

\end{run}

\needspace{12em}
One can filter a |Plist| with a |pNest| such that only those list
items prevail, which are part of the |pNest|.

\begin{code}
lFilter :: (Ord a) => Pnest a -> Plist a -> Plist a
lFilter pn PlAny = Plist [pn]
lFilter pn (Plist pns) = Plist $ foldr f [] pns
  where
    f pn' ys = case nIntersection pn pn' of
      (Just y') -> y':ys
      otherwise -> ys
\end{code}

\needspace{12em}
\begin{run} 
We can filter our |ex_plist1| such that only "foo1" inside a "foo" are
allowed. No more "bar" toplevels and no more "foo2" inside a "foo" are
accepted.

|*Main> lFilter (Pnest "foo" (Plist [Pnest "foo1" PlAny])) ex_plist1|
  \eval{lFilter (Pnest "foo" (Plist [Pnest "foo1" PlAny])) ex_plist1}
\end{run}


\needspace{12em}
Finally the intersection of |Plist|. Basically we build the union of
filtering the second |Plist| by every |pNest| in the first |Plist|.

\begin{code}
lIntersection :: Ord a => Plist a -> Plist a -> Plist a
lIntersection PlAny x  = x
lIntersection x PlAny  = x
lIntersection (Plist pcks1) pls = foldr lUnion (Plist []) $ do
  pck1 <- pcks1
  return $ lFilter pck1 pls
\end{code}


\subsection{Processes transforming Products}

Because we destinguished between |Plist| and |Pnest| we can now be
specific about what kind of Product each Process transforms and what
the type of the transformed Product is. It will not be possible to
accidently place a |Split| afer a |Pack|, because (as we shall see)
|Split| computes a |Plist|, which is not transformed by a |Pack|.


\subsubsection{Split}

|Split| takes a number of |Plists|, which stand for the Products which
are accepted e.g. by the Stackers, and computes the Product which is
accepted by the feeder of the machine. The computed Product is again a
|Plist|.

\begin{code}
split :: (Ord a)=> [Plist a] -> Plist a
split pxs = foldr lUnion (Plist []) pxs
\end{code}

\subsubsection{Merge}

|Merge| does the inverse operation, but as stated earlier, it needs
additional information to decide, what to accept at each input. This
additional information comes in the form of a list of |Plist|, one
for each input. The result of the computation is a list of |Plist|,
again one for each input.

\begin{code}
merge :: (Eq a, Ord a) => [Plist a] -> Plist a -> [Plist a]
merge plss pls = map f plss
  where
    f p = lIntersection p pls

-- xxx
mergeAll :: (Eq a, Ord a) => Plist a ->[Pnest a]
mergeAll (Plist pns) = pns

\end{code}

\subsubsection{Pack} |Pack| takes a |Pnest| and computes the
container label and the |Plist| for the items it accepts. Essentially
it removes one level of nesting.

\begin{code}
pack :: Pnest lty -> (lty, Plist lty)
pack (Pnest lbl pls) = (lbl, pls)
\end{code}

\subsubsection{Unpack}
|Unpack| takes a container label and a |Plist| and produces a |Pnest|.


\begin{code}
unpack :: lty -> Plist lty -> Pnest lty
unpack lbl pls = Pnest lbl pls
\end{code}

\section{Larger examples}

I got stuck here. Consider a rollcontainer containing trays for 10
routes. You unpack the rollcontainer and then split the trays into two
parts, one containing the odd routes and the other containing the even
routes. You cannot sensibly unpack any of the outputs, because Unpack
assumes all containers carry the same label. Only when you split the
trays into 10 routes you get ``pure'' trays which can be
unpacked. Could it be that there is a special Split operation, which
splits by label?

Something is not right with our elementary functions.

\begin{figure}[htb!]
\centering
\includegraphics[width=12cm]{ProductsExRec.eps}
\caption{Pack}
\end{figure}

\begin{code}
leaf :: lty -> Plist lty
leaf lbl = Plist[Pnest lbl PlAny]

splitAll :: Ord a => [Pnest a] -> Plist a
splitAll ns = split $ map (\np -> Plist [np]) ns
\end{code}

\begin{code}
type Ex_lbl = (String, String, String)
ex_truck = 
        let 
           --shorthands
           lbl s x y = s ++ (show x) ++ (show y)
           clbl s1 s2 n = (s1, s2 ++ (show n), "")
           range size n = [size*n .. size*(n+1)-1]
           ------------------------------------------------------------
           -- Receiver side
           ------------------------------------------------------------
           -- The nth route consists of the following addresses
           -- xxx split ord and prio
           rRoute route = split [leaf("Letter", lbl "Addr" route i, mclass)
                                         | i <-[1..10],
                                               mclass <- ["Ord", "Prio"]
                               ] :: Plist Ex_lbl
           -- Each route has its own |Pnest| tray product
           rTray route   = unpack (clbl "Tray" "Route" route) (rRoute route)      
                         :: Pnest Ex_lbl

           -- Each delivery office is responsible for 10 routes ...
           rDof dof    = splitAll [rTray route | route <- range 10 dof] 
                       :: Plist Ex_lbl

           -- ... and has a dedicated rollcontainer
           rRc n     = unpack (clbl "RollContainer" "DO" n) (rDof n)  
                     :: Pnest Ex_lbl

           -- a Region services 5 Delivery Offices ...
           rRegion reg = splitAll [rRc i | i <- range 5 reg]     
                       :: Plist Ex_lbl

           -- ... and has a truck bringing the rollcontainers
           rTruck n  = unpack (clbl "Truck" "Region" n) (rRegion n)   :: Pnest Ex_lbl

           ------------------------------------------------------------
           -- Now what happens at the departure side of the truck
           ------------------------------------------------------------
           -- The truck to region n gets packed
           sTruck n = pack(rTruck n)               :: (Ex_lbl, Plist Ex_lbl)

           -- Each rollcontainer comes from a different source ...
           sRcs reg  = mergeAll (snd (sTruck reg))     :: [(Pnest Ex_lbl)]

           -- ... where they get packed
           sDof reg dof  = pack (sRcs reg !! dof)          :: (Ex_lbl, Plist Ex_lbl)

           -- 

           in (sRcs 0) -- (stray 0) !! 0

\end{code}
