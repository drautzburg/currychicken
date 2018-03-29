{-# LANGUAGE DeriveFunctor #-}
import Data.Function
import Data.Monoid
import Text.Pretty.Simple
import Control.Applicative

data Track a = Track {
    lst ::ZipList a
    }
    deriving (Eq, Show, Functor)

track = Track . ZipList

instance Applicative Track where
    pure a = track [a]
    (Track tf) <*> (Track ta) = Track (tf <*> ta)


instance Monoid (ZipList a) where
    mempty  = ZipList []
    mappend l1 l2 = ZipList $ mappend  (getZipList l1)  (getZipList l2)


instance Monoid (Track a) where
    mempty = track []
--    mappend l1 l2 = track $ mappend lst  (lst l2)
--    mappend t1 t2 = track $ mappend (getZipList $ lst t1)  (getZipList $ lst t2)
--    mappend = liftA2 mappend
    mappend l1 l2 = Track $ mappend  (lst l1)  (lst l2)


xx1 = [1..3]
xx2 = [10..13]
xxx = ZipList xx1

instance Foldable Track where
    foldr f b0 tr = foldr f b0 (lst tr)


instance Traversable Track where
    traverse f = fmap Track .  traverse f . lst
    
type Dur = Rational
type Time = Double

type OrdTrack a = Track a
type DurTrack a = Track (Dur, a)
type TimTrack a = Track (Time,a)

exDur :: DurTrack String
exDur = track $ do
    d <- [1..5]
    return (d, show d)

schedule :: (Dur->Time) -> Time -> DurTrack a -> TimTrack a
schedule scale t0 tr = let f t dt = t + scale dt
                       in track $ scanl (ff f) (t0, undefined) (getZipList $ lst tr)

ff :: (t->d->t) -> (t,x) -> (d,x) -> (t,x)
ff f (t,_) (d,x) = (f t d, x)

data Part = Intro | Verse | Chorus | Bridge
    deriving (Eq, Show)

song = track [Intro, Verse, Verse, Chorus, Verse, Chorus, Bridge, Chorus, Chorus]

number' :: (Eq a) => (a->Int) -> [a] -> [(a,Int)]
number' _ [] = []
number' next (a:as) = let n = next a
                          next' a' = if a==a' then n+1 else next a'
                      in (a, n) : number' next' as



data Chord = C | D | E | F deriving (Eq, Show)

chords tr = let f p = case p of
                 (Intro,_) -> (p,track [C])
                 _         -> (p,track [D,E,D])
         in fmap f tr

-- ex = chords . number $ song

