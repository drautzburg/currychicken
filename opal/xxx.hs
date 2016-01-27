{-# Language FlexibleInstances #-}
{-# Language UndecidableInstances #-}

import qualified Data.List as L
import qualified Data.Set as S
import Debug.Trace
import Test.QuickCheck hiding ((==>))

------------------------------------------------------------
class Poset p where
------------------------------------------------------------
        (<:) :: p -> p -> Bool

-- Instances of Poset
instance Poset Int where (<:) = (==)

------------------------------------------------------------
-- Alternatives
------------------------------------------------------------
newtype PsList a = PsList [a]
newtype PsSet  a = PsSet (S.Set a)

isSubPolist :: (Poset a) => [a] -> [a] ->Bool
isSubPolist as bs = all inside_some_b as
        where
            inside_some_b a = any (a <:) bs

instance (Eq a, Ord a, Poset a) => Poset (PsList a)
        where
            (PsList as) <: (PsList bs) = isSubPolist as bs

instance (Eq a, Ord a, Poset a) => Poset (PsSet a)
        where
            (PsSet as) <: (PsSet bs) = isSubPolist (S.toList as) (S.toList bs)


------------------------------------------------------------
data Crust a = Open [a] | Closed [a]
------------------------------------------------------------
             deriving (Eq, Ord, Show)

instance (Eq a, Ord a, Poset a) => Poset (Crust a) 
        where 
            (<:) (Open as) (Closed bs)   = False
            (<:) (Closed as) (Closed bs) = as == bs

            (<:) (Open _) (Open [])         =  True
            (<:) (Open []) (Open _)         =  False
            (<:) (Open (x:xs)) (Open (y:ys)) = x <: y && 
                                               (Open xs) <: (Open ys)

            (<:) (Closed _) (Open [])   = True
            (<:) (Closed []) (Open _)   = False
            (<:) (Closed (x:xs)) (Open (y:ys))  = x <: y && 
                                                  (Closed xs) <: (Open ys)


