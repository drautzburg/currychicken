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
import Control.Applicative
import Debug.Trace
import Test.QuickCheck hiding ((==>))
import GHC.Read
import Text.ParserCombinators.ReadPrec
import qualified Text.Read.Lex as L
------------------------------------------------------------
class Poset p where
------------------------------------------------------------
        (<:) :: p -> p -> Bool
        -- Posets are not Eq, because that would conflict with the
        -- derived Eq instances
        equals :: p -> p -> Bool
        equals a b = a <: b && b <: a



------------------------------------------------------------
newtype PsAtom a =  A {val :: a}
        deriving (Read, Show)
------------------------------------------------------------
-- PsAtoms are things where <: is the same as ==
instance (Eq a, Ord a) => Poset  (PsAtom a) where (<:) (A a) (A b) = a == b

instance (Eq a, Ord a) => Eq     (PsAtom a) where 
        (==) (A a) (A b) = a == b
instance (Eq a, Ord a) => Ord    (PsAtom a) where (<=) (A a) (A b) = a <= b
instance                 Functor PsAtom     where fmap f (A a) = A (f a)
instance Applicative PsAtom where 
        (A f) <*> (A a) = A (f a)
        pure f          = A f
------------------------------------------------------------
-- Alternatives/Sets
------------------------------------------------------------
-- Collections of Posets are Posets themselves

newtype PsList a = PsList [a] deriving (Ord, Eq, Show)
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


------------------------------------------------------------
data Crust a = Open [a] | Closed [a] deriving (Show)
------------------------------------------------------------
instance Functor Crust where
        fmap f (Open as)   = Open (fmap f as)
        fmap f (Closed as) = Closed (fmap f as)


-- Crusts of Posets are Posets themselves
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
compress :: Ord (PsSet a) => PsSet a -> PsSet (PsSet a)
compress (PsSet ps) = PsSet $ S.map (PsSet . S.singleton) ps


------------------------------------------------------------
-- Tests
------------------------------------------------------------

instance (Arbitrary a) => Arbitrary (Crust (PsAtom a))
        where
            arbitrary = do
                xs <- arbitrary 
                elements [
                         Open $ map A xs, 
                         Closed $ map A xs
                        ]


a ==> b = not a || b 
infixr 1 ==>

type IntCrust = Crust (PsAtom Int)

prop_reflexive :: IntCrust -> Bool
prop_reflexive as = as <: as


prop_anti :: IntCrust -> IntCrust -> Bool
prop_anti as bs = as <: bs && bs <: as ==> as `equals` bs

prop_trans :: IntCrust -> IntCrust -> IntCrust -> Bool
prop_trans as bs cs = as <: bs && bs <: cs ==> as <: cs


onList f (Open as)   = Open   $ f as
onList f (Closed as) = Closed $ f as


prop_subset ::  PsAtom Int -> IntCrust -> IntCrust -> Bool
prop_subset a as bs 
        | as <: bs = as <: (onList (a:) bs)
        | bs <: as = bs <: (onList (a:) as)
        | otherwise = True


prop_lift :: IntCrust -> IntCrust -> Bool
prop_lift as bs = (as `equals` bs) ==> (fmap l as) `equals` (fmap l bs)
        where
            l x = PsList [x]


runTest name test = putStrLn "" >> putStrLn name >> quickCheck test

testAll = do
    runTest "reflexive" prop_reflexive
    runTest "transitive"     prop_trans
    runTest "antisymmetric"      prop_anti
    runTest "lift"      prop_lift


{-
instance (Show a) => Show (Crust a) where
        show (Closed []) = "<>"
        show (Closed (x:xs)) = "<" ++ show x ++ foldr ((++) . (","++). show) "" xs ++ ">"
        show (Open []) = "<..>"
        show (Open (x:xs)) = "<" ++ show x ++ foldr ((++) . (","++). show) "" xs ++ "..>"



instance (Read a) => Read (Crust a) where
        readsPrec _ s = readsCrust s
                where
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
-}
