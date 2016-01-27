{-# Language FlexibleInstances #-}
{-# Language UndecidableInstances #-}
{-# Language FlexibleContexts #-}
{-

Attempt to see Items not from outside (Containers with their content),
but as innermost items and were they are contained in ("Crust"). This
seems more basic to me, The outside-in view seems to pre-assume a
certain compression.

(<:) answeres whether one item is contained in another. This relation
forms a partial order. My hope is, that it will be easy to extend this
notions to Crusts on Lists/Sets. This would be a compressed version of
the original Crust.

-}

import qualified Data.List as L
import qualified Data.Set as S
import Debug.Trace
import Test.QuickCheck hiding ((==>))
import GHC.Read
import Text.ParserCombinators.ReadPrec
import qualified Text.Read.Lex as L
------------------------------------------------------------
class Poset p where
------------------------------------------------------------
        (<:) :: p -> p -> Bool

-- Instances of Poset
instance Poset Int where (<:) = (==)

{-
instance (Eq a, Eq b) => Poset (a,b) 
        where (<:) = (==)
-}
------------------------------------------------------------
-- Alternatives
------------------------------------------------------------
newtype PsList a = PsList [a]
newtype PsSet  a = PsSet (S.Set a) deriving Show

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

{-
instance (Ord p, Poset p) => Eq (PsList p)
         where as == bs = as <: bs && bs <: as

instance (Ord p, Poset p) => Eq (PsSet p)
         where as == bs = as <: bs && bs <: as


instance (Eq a) => Poset a
        where
            (<:) = (==)
-}

{-
------------------------------------------------------------
newtype PsAtom a =  A a
        deriving (Show)
------------------------------------------------------------
-- The only way an atom can <: another atom is to be equal
instance (Eq a, Ord a) => Poset (PsAtom a) where (<:) (A a) (A b) = a == b
instance (Eq a, Ord a) => Eq    (PsAtom a) where (==) (A a) (A b) = a == b
instance (Eq a, Ord a) => Ord   (PsAtom a) where (<=) (A a) (A b) = a <= b
-}

compress :: Ord (PsSet a) => PsSet a -> PsSet (PsSet a)
compress (PsSet ps) = PsSet $ S.map (PsSet . S.singleton) ps
------------------------------------------------------------
data Crust a = Open [a] | Closed [a]
------------------------------------------------------------
             deriving (Eq, Ord)


instance (Show a) => Show (Crust a) where
        show (Closed []) = "<>"
        show (Closed (x:xs)) = "<" ++ show x ++ foldr ((++) . (","++). show) "" xs ++ ">"
        show (Open []) = "<..>"
        show (Open (x:xs)) = "<" ++ show x ++ foldr ((++) . (","++). show) "" xs ++ "..>"


instance (Read a) => Read (Crust a) where
        readsPrec _ s = readsCrust s

readsCrust ::  (Read a) => ReadS (Crust a)
readsCrust ('<':s)       =  do 
    (x,  t)          <- reads s
    ((crust, xs), u) <- readsL t
    return (crust (x:xs),u)
        where
            readsL (',':s) = do
                                (y, t) <- reads s
                                ((t,ys),u) <- readsL t
                                return ((t, y:ys), u)

            readsL ('>':s)         = return ((Closed,[]),s) 
            readsL ('.':'.':'>':s) = return ((Open, []),s)




instance (Eq a, Ord a, Poset a) => Poset (Crust a) 
        where 
            (<:) (Open as) (Closed bs)   = False
            (<:) (Closed as) (Closed bs) = as == bs

            (<:) (Open _) (Open [])     =  True
            (<:) (Open []) (Open _)     =  False
            (<:) (Open (x:xs)) (Open (y:ys)) 
                                        = x <: y && 
                                          (Open xs) <: (Open ys)

            (<:) (Closed _) (Open [])   = True
            (<:) (Closed []) (Open _)   = False
            (<:) (Closed (x:xs)) (Open (y:ys))  
                                        = x <: y && 
                                          (Closed xs) <: (Open ys)


------------------------------------------------------------
-- Tests
------------------------------------------------------------

instance (Arbitrary a) => Arbitrary (Crust a)
        where
            arbitrary = do
                xs <- arbitrary 
                elements [Open xs, Closed xs]

a ==> b = not a || b 
infixr 1 ==>

prop_reflexive :: Crust Int -> Bool
prop_reflexive as = as <: as

prop_anti :: Crust Int -> Crust Int -> Bool
prop_anti as bs = as <: bs && bs <: as ==> as == bs

prop_trans :: Crust Int -> Crust Int -> Crust Int -> Bool
prop_trans as bs cs = as <: bs && bs <: cs ==> as <: cs


onList f (Open as)   = Open   $ f as
onList f (Closed as) = Closed $ f as

prop_subset ::  Int -> Crust Int -> Crust Int -> Bool
prop_subset a as bs 
        | as <: bs = (trace "-- a --") $ as <: (onList (a:) bs)
        | bs <: as = (trace "-- b --") bs <: (onList (a:) as)
        | otherwise = True


runTest name test = putStrLn "" >> putStrLn name >> quickCheck test

testAll = do
    runTest "reflexive" prop_reflexive
    runTest "trans"     prop_trans
    runTest "anti"      prop_anti





ex_crust1 = [
 Closed [1,2,3],
 Closed [1,2,4]
 ]

ex_crust2 = [
 Closed [1,2,3],
 Closed [1,2]
 ]

