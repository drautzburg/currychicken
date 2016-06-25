{-# LANGUAGE BangPatterns#-}


import Data.Monoid
import Control.Monad.State.Strict
import System.TimeIt
import Text.Show.Pretty
import Debug.Trace
import Control.DeepSeq
import Data.Sequence

ts x = trace ("tr="++show x) x
------------------------------------------------------------
-- Simple Time Stuff
------------------------------------------------------------
type Instant = Double
type Interval = Double
type Timed a = (Instant, a)
type Log a = [Timed a]

instant = fst
before tx (t,a) = t <= tx



type Logger log a = a -> log -> log

lgr :: Logger [Timed Int] (Timed Int)
lgr a log = a : log

data Acc a b = Acc {runAcc :: Log a -> (Log a, Maybe b)}

instance Functor (Acc a) where
        fmap f (Acc acc) = Acc $ \log -> 
                           case acc log of
                               (la, Just lb) -> (la, Just (f lb))
                               (la, Nothing) -> (la, Nothing)

instance Applicative (Acc a) where
        pure x = Acc $ \log -> (log, Just x)
        (Acc x) <*> (Acc y) = Acc $ \log ->
                          case x log of
                              (log', Nothing) -> (log', Nothing)
                              (log', Just f)  -> 
                                      case y log' of
                                          (log'', Nothing) -> (log'', Nothing)
                                          (log'', Just z)  -> (log'', Just $ f z)
                                                     

sumTo :: Instant -> Acc Int (Timed Int)
sumTo tx = Acc $ \log -> let (old, new) = span (before tx) log
                             !s         = sum $ map snd old
                         in (new, Just (tx,s))

sumBy :: Instant -> Instant -> Instant -> Acc Int (Log Int)
sumBy tmax dt tx
      | tx > tmax = pure []
      | otherwise = (:) <$>  sumTo tx <*> sumBy tmax dt (tx+dt) 




-- n=10^6 :: Double
n=10^2      :: Double

ex_data = [(t,1) | t<- [1..10*n]]

ex_data2 = let xs i = if i == (n+5) then [] else (i,1): (xs (i+1))
           in xs 1


--x = runAcc (sumTo 2)  ex_data
x = let agg = n/10
    in runAcc (sumBy n agg agg) ex_data2

main = timeIt $ putStrLn $ show x
