{-# LANGUAGE ScopedTypeVariables #-}
import Data.Ratio
import Data.Tuple

-- A Rhythm tells you when to do how much
type Duration = Rational
type Instant  = Rational
type Rhythm a   = [(Duration, a)]
type Schedule a = [(Instant, a)]


type Pair x a = (x,a)

onFst :: (x->y) -> (x,a) -> (y,a)
onFst f (x,a) = (f x, a)

onSnd :: (a->b) -> (x,a) -> (x,b)
onSnd f (x,a) = (x, f a)

-- transform a function between Pairs of Lists to one between List of Pairs

raise :: (t -> Pair [x] [a] -> Pair [y] [b])
       -> t -> [Pair x a]   -> [Pair y b]
raise on f = uncurry zip . on f . unzip

schedule :: Instant -> Rhythm a -> Schedule a
schedule = raise onFst . scanl (+) 

{-
We can generate all possible Rhythms so they are ordered by
"weirdness", or any other metric. A metric is computed by counting and
weighing the operations it took to generate the Rhythm. By supplying
your own weighing function, you can set your preferences concerning
the way you choose your Rhythms.
-}

------------------------------------------------------------
data Lick a= Whole a
------------------------------------------------------------
             | Stretch Rational (Lick a)
             | Repeat Int (Lick a)
             | Append (Lick a) (Lick a)
    deriving (Eq, Show)

-- from this we can create Rhythms
type Intensity = Double

oddness :: Rational -> Double
oddness r = fromIntegral $ numerator r + denominator r

duration :: (Lick a) -> Duration
duration = foldr (+) 0 . map fst . render 

render :: Lick a -> Rhythm a
render (Whole a)      = [(1,a)]
render (Stretch k l)  = (map . onFst . (*) . fromRational) k (render l)
render (Repeat 0 l)   = render l
render (Repeat n l)   = render $ Append l (Repeat (n-1) l)
render (Append l1 l2) = render l1 ++ render l2

reduce :: Eq a => Lick a -> Lick a
reduce (Whole a) = Whole a

reduce (Stretch kx lx) = case reduce lx of
    Stretch ky ly | kx == (1/ky) -> reduce ly
    _                            -> Stretch kx (reduce lx)


reduce (Repeat nx lx) = case reduce lx of
    Repeat ny ly ->  Repeat (nx+ny) (reduce ly)
    _            ->  Repeat nx (reduce lx)

reduce (Append lx ly) = let (lx', ly') = (reduce lx, reduce ly)
                        in case (lx', ly') of
                               (Stretch k1 l1, Stretch k2 l2) | k1 == k2
                                                          -> Stretch k1 $ reduce (Append l1 l2)
                               (Repeat n1 l1, l2) | l1 == l2 -> Repeat (n1+1) l1
                               (l1, Repeat n1 l2) | l1 == l2 -> Repeat (n1+1) l1
                               (Repeat n1 l1, Repeat n2 l2) | l1 == l2
                                                             -> Repeat (n1+n2) l1    
                               _ | lx' == ly'                -> Repeat 1 lx'
                               _                             -> Append lx' ly'
        



stdCost :: Eq a => Lick a -> Double
stdCost lx = case reduce lx of
    Whole _       -> 1
    Stretch ky ly -> 1 + oddness ky + stdCost ly
    Repeat n ly   -> 1 + stdCost ly
    Append ly lz  -> 1 + stdCost ly + stdCost lz


stretches :: Lick a -> [Lick a]
stretches l = map ($ l) [Stretch k | k <- [1,2,1/2,3,1/3]]

repeats :: Lick a -> [Lick a]
repeats l = map ($ l) [Repeat n | n <- [1..8]]

appends :: [Lick a] -> [Lick a]
appends [] = []
appends (l:ls) = l : map (Append l) (appends ls)

rhythms :: (Lick a -> Double) -> [Lick a] -> [Lick a]
rhythms costFun ls = do
    l <- ls
    r <- repeats l
    s <- stretches l
    a <- appends ls
    undefined
    
    


ex1 =  (Append . Stretch (1/4) . Repeat 3 . Whole $ ()) (Stretch (1/4) . Repeat 3 . Whole $ ())
