{-# Language FlexibleInstances #-}
{-# Language FlexibleContexts #-}
{-# Language StandaloneDeriving #-}
{-# Language UndecidableInstances #-}

import qualified Data.Set as S
import qualified Data.List as L
import qualified Data.Monoid as M
import Control.Applicative
import qualified Data.Foldable as F
import Test.QuickCheck hiding ((==>))



------------------------------------------------------------
class PartialOrder po where
------------------------------------------------------------
        (<:) :: (Ord a) => po a -> po a -> Bool

instance PartialOrder S.Set
        where
            (<:) = S.isSubsetOf

-- xxx to compare products and items we must extract the relevant
-- parts of the item label. We are comaring two different things:
-- items and Products

instance PartialOrder []
        where
            as <: bs = (L.sort as) `L.isPrefixOf` (L.sort bs)


{-
------------------------------------------------------------
class F.Foldable sl => SetLike sl where
------------------------------------------------------------
        union :: sl a -> sl a -> sl a
-}
------------------------------------------------------------
data Nested grp a = Nested Ending [grp a] 
------------------------------------------------------------
                 deriving (Eq, Show)

data Ending = Open | Closed deriving (Eq, Show)


-- A nested a is a partial order if grp is a partial order
instance (PartialOrder grp) => PartialOrder (Nested grp) 
        where (<:) = contains

instance (PartialOrder grp, Functor grp) => Functor (Nested grp) 
        where
            fmap f (Nested end as) = Nested end (map (fmap f) as)


contains :: (PartialOrder grp, Ord a) => Nested grp a -> Nested grp a -> Bool

contains (Nested _ _ ) (Nested Open []) = True
contains (Nested e xs ) (Nested Closed []) =
        e == Closed && 
        null xs
contains (Nested e1  (a : as)) (Nested e2  (b : bs)) =
        (a <: b) && (Nested e1 as) `contains` (Nested e2 bs)
contains _ _ = False


equalsn :: (PartialOrder grp, Ord a) => Nested grp a -> Nested grp a -> Bool
equalsn as bs = as <: bs && bs <: as


------------------------------------------------------------
data Items ors grp a = Items (ors (Nested grp a)) 
                       
deriving instance Show (ors (Nested grp a)) => Show (Items ors grp a)
------------------------------------------------------------


-- Items being a collection (ors) of Nested is a PartialOrder if grp
-- is a PartialOrder

instance   (F.Foldable ors, PartialOrder grp) =>  PartialOrder (Items ors grp)
        where 
            (Items as) <: (Items bs) = F.foldr allMatch True as
                    where
                        allMatch a False = False
                        allMatch a True  = F.foldr (anyMatch a) False bs
                        anyMatch a b True = True
                        anyMatch a b False = a <: b


-- Functor (Nested grp) 
instance (PartialOrder grp, Functor ors, Functor grp) =>  Functor (Items ors grp)
        where
            fmap f (Items nests) = Items $ fmap (fmap f) nests


equalsi :: (PartialOrder grp, F.Foldable ors, Ord a) => Items ors grp a -> Items ors grp a -> Bool
equalsi as bs = as <: bs && bs <: as

------------------------------------------------------------
-- Tests
------------------------------------------------------------
instance (Arbitrary a) => Arbitrary (Nested [] a)
        where
            arbitrary = do
                xs <- arbitrary 
                e <- elements [Open, Closed]
                return (Nested e xs)


instance (Arbitrary a) => Arbitrary (Items [] [] a)
        where
            arbitrary = do
                xs <- arbitrary 
                return (Items xs)

        -- xxx = sample (arbitrary :: Gen (Nested [] Int))

-- logical implication
a ==> b = not a || b 
infixr 1 ==>

type IntNest = Nested [] Int 
type IntItems = Items  [] [] Int

prop_reflexiven :: IntNest -> Bool
prop_reflexiven as = as <: as

prop_reflexivei :: IntItems -> Bool
prop_reflexivei as = as <: as

prop_transn :: IntNest -> IntNest -> IntNest -> Bool
prop_transn as bs cs = as <: bs && bs <: cs ==> as <: cs

prop_transi :: IntItems -> IntItems -> IntItems -> Bool
prop_transi as bs cs = as <: bs && bs <: cs ==> as <: cs


prop_antin :: IntNest -> IntNest -> Bool
prop_antin as bs = as <: bs && bs <: as ==> as `equalsn` bs

prop_antii :: IntItems -> IntItems -> Bool
prop_antii as bs = as <: bs && bs <: as ==> as `equalsi` bs


{-
prop_subset ::  Int -> IntNest -> IntNest -> Bool
prop_subset a as bs 
        | as <: bs = as <: (onList (a:) bs)
        | bs <: as = bs <: (onList (a:) as)
        | otherwise = True
-}

testAll = do
    runTest "reflexive nests"     prop_reflexiven
    runTest "reflexive items"     prop_reflexivei
    runTest "transitive nests"    prop_transn
    runTest "transitive items"    prop_transi
    runTest "antisymmetric nests " prop_antin
    runTest "antisymmetric items " prop_antii


--     runTest "lift"      prop_lift
    where
       runTest name test = putStrLn "" >> putStrLn name >> quickCheck test
