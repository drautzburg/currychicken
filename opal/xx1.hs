{-# Language FlexibleInstances #-}
{-# Language UndecidableInstances #-}

import qualified Data.List as L
import qualified Data.Set as S
import Debug.Trace
import Test.QuickCheck hiding ((==>))
import Data.Ord
import Data.Monoid

ts x = trace ("==> " ++ show x)

------------------------------------------------------------
class Set s where
------------------------------------------------------------
        union :: (Ord a) => s a -> s a -> s a
        inter :: (Ord a) => s a -> s a -> s a
        singl ::               Crust a  -> s a



------------------------------------------------------------
data Ending = Open | Closed deriving (Eq, Show)
data Crust a = Crust Ending [a] deriving (Eq, Show)
------------------------------------------------------------
isIn (Crust _ _ ) (Crust Open []) = True
isIn (Crust e xs ) (Crust Closed []) = 
        null xs
isIn (Crust e1  (a:as)) (Crust e2  (b:bs)) =
        (a == b) && 
        Crust e1 as `isIn` Crust e2 bs
isIn _ _ = False


instance Ord Ending where
        compare Open Closed = GT
        compare Closed Open = LT
        compare _ _         = EQ

instance (Eq lty, Ord lty) => Ord (Crust lty) where
        -- sorts most general value last
        compare (Crust e1 as) (Crust e2 bs) = compare (Down as) (Down bs) <> compare e1 e2 


------------------------------------------------------------
data CrustSet lty = CrustSet (S.Set (Crust lty)) deriving Show
------------------------------------------------------------

onSet f (CrustSet s1) (CrustSet s2) = CrustSet (f s1 s2)

instance Set CrustSet where
        union = onSet S.union
        inter = onSet S.intersection
        singl x = CrustSet $ S.singleton x
                      

ex_s1 = singl (Crust Open [1,2,3]) :: CrustSet Int
ex_s2 = singl (Crust Open [1,2,4]) :: CrustSet Int


------------------------------------------------------------
data CrustList lty = CrustList [(Crust lty)] deriving Show
------------------------------------------------------------



onList f (CrustList s1) (CrustList s2) = CrustList (f s1 s2)

instance Set CrustList where
        union = onList L.union
        inter = onList L.intersect
        singl x = CrustList [x]



clUnion (CrustList as) (CrustList bs) = fst ys : snd ys
        where
            xs = L.sort (as ++ bs)
            ys = foldr f (Crust Closed [], []) xs
            f cx (cacc, yacc)
                    | cx `isIn` cacc = (cacc, yacc)
                    | otherwise      = if cacc == Crust Closed [] 
                                       then (cx, yacc)
                                       else (cx, cacc:yacc)

ex_l1 = singl (Crust Open [1,2,3]) :: CrustList Int
ex_l2 = CrustList [Crust Open [1,2], Crust Closed [1,2,3]] :: CrustList Int
