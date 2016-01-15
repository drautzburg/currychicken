import qualified Data.List as L
import qualified Data.Set as S
import qualified Data.Map as M
import Text.Show.Pretty


import Data.Maybe
import Data.Function
import Control.Monad
import Debug.Trace

pp x = putStrLn $ show x

data Lbl a b = Lbl a b 

type Range a b = M.Map a (S.Set (b,b))

addLbl :: (Ord a, Ord b) => S.Set b -> Lbl a b -> Range a b -> Range a b
addLbl allBs (Lbl a b) rs = case M.lookup a rs  of
                                Nothing -> M.insert a (S.singleton (b,b)) rs
                                Just bs -> case S.lookupLE (b,b) bs of
                                               Nothing -> undefined

addRange :: (Ord a, Ord b) => S.Set b -> a -> (b,b) -> Range a b -> Range a b
addRange allBs a (l,h) rs = case M.lookup a rs  of
                                Nothing -> M.insert a (S.singleton (l,h)) rs
                                Just bs -> let (++) = S.union
                                               (below,_)  = S.split (l,l) bs
                                               (_, above) = S.split (h,h) bs
                                           in M.insert a (below ++ above ++ S.singleton (l,h)) rs


mergeLower :: S.Set b -> b -> S.Set (b,b) -> S.Set (b,b)
mergeLower allBs b bs = undefined

isGap :: (Ord a, Show a) => S.Set a -> a -> a -> Bool
isGap xs l h = case S.lookupGT l xs of
                   Nothing -> True
                   Just x  -> x >= h

ex_b = S.fromList $ join [[n..n+10] | n <- [0,20..100]]
ex_r = M.fromList [
        ("foo",
              S.fromList[
                        (1,10),
                        (20,30)
                       ]
        ),
        ("bar",
              S.fromList[
                        (1,10),
                        (20,30)
                       ]
        )
       ]

xxx = addRange ex_b "foo" (10,12) ex_r
