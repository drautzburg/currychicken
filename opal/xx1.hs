{-# Language FlexibleInstances #-}
{-# Language UndecidableInstances #-}

import qualified Data.List as L
import qualified Data.Set as S
import Debug.Trace
import Test.QuickCheck hiding ((==>))
import Data.Ord

ts x = trace (show x)

------------------------------------------------------------
class Set s where
------------------------------------------------------------
        union :: (Ord a) => s a -> s a -> s a
        inter :: (Ord a) => s a -> s a -> s a
        singl :: a -> s a


instance Set S.Set where
        union = S.union
        inter = S.intersection
        singl = S.singleton

------------------------------------------------------------
data Crust a = Open [a] | Closed [a] deriving (Show)
-- xxx can do inter but not union or singl
------------------------------------------------------------

type Product set lty = set (Crust lty)



ex_1 :: Product (S.Set) Int
ex_1 = (singl $ Open [3, 1,2])


------------------------------------------------------------
data Sop a = Sop [(a, [a])] deriving Show
------------------------------------------------------------
instance Set Sop where
        singl x = Sop [(x,[])]
        union (Sop xs) (Sop ys) = Sop (snd zs)
                where
                    zs = foldr f (head xx, []) xx
                    f (a,as) ((b:bs), ys)
                            | a == b = ((a, as++bs), ys)


                    xx = L.sortBy (comparing fst) (xs ++ ys)

