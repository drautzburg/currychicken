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
        singl ::               Wrapped a  -> s a



------------------------------------------------------------
data Ending = Open | Closed deriving (Eq, Show)
data Wrapped a = Wrapped Ending [a] deriving (Eq, Show)
------------------------------------------------------------
isIn (Wrapped _ _ ) (Wrapped Open []) = True
isIn (Wrapped e xs ) (Wrapped Closed []) = 
        null xs
isIn (Wrapped e1  (a:as)) (Wrapped e2  (b:bs)) =
        (a == b) && 
        Wrapped e1 as `isIn` Wrapped e2 bs
isIn _ _ = False


instance Ord Ending where
        compare Open Closed = GT
        compare Closed Open = LT
        compare _ _         = EQ

instance (Eq lty, Ord lty) => Ord (Wrapped lty) where
        -- sorts most general value last
        compare (Wrapped e1 as) (Wrapped e2 bs) = compare (Down as) (Down bs) <> compare e1 e2 


------------------------------------------------------------
data WrappedSet lty = WrappedSet (S.Set (Wrapped lty)) deriving Show
------------------------------------------------------------

onSet f (WrappedSet s1) (WrappedSet s2) = WrappedSet (f s1 s2)

instance Set WrappedSet where
        union = onSet S.union
        inter = onSet S.intersection
        singl x = WrappedSet $ S.singleton x
                      

ex_s1 = singl (Wrapped Open [1,2,3]) :: WrappedSet Int
ex_s2 = singl (Wrapped Open [1,2,4]) :: WrappedSet Int


------------------------------------------------------------
data WrappedList lty = WrappedList [(Wrapped lty)] deriving Show
------------------------------------------------------------



onList f (WrappedList s1) (WrappedList s2) = WrappedList (f s1 s2)

instance Set WrappedList where
        union = onList L.union
        inter = onList L.intersect
        singl x = WrappedList [x]



clUnion (WrappedList as) (WrappedList bs) = fst ys : snd ys
        where
            xs = L.sort (as ++ bs)
            ys = foldr f (Wrapped Closed [], []) xs
            f cx (cacc, yacc)
                    | cx `isIn` cacc = (cacc, yacc)
                    | otherwise      = if cacc == Wrapped Closed [] 
                                       then (cx, yacc)
                                       else (cx, cacc:yacc)

ex_l1 = singl (Wrapped Open [1,2,3]) :: WrappedList Int
ex_l2 = WrappedList [Wrapped Open [1,2], Wrapped Closed [1,2,3]] :: WrappedList Int
