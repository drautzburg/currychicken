{-# Language FlexibleInstances #-}
{-# Language FlexibleContexts #-}
{-# Language StandaloneDeriving #-}
import Data.Monoid
import Data.List
import Data.Ord
import qualified Data.Foldable as F
import Data.Tuple
import Control.Exception
import qualified Data.Set as S
import Data.Function



-- | To be Aggregatable you need to provide these functions
class Agg pair where
        cmpSnd :: (Ord (coll (pair coll a))) => 
                  pair coll a -> pair coll a -> Ordering

        cmpFst :: (Ord a) => 
                  pair coll a -> pair coll a -> Ordering

        aggFst :: (Monoid a, Eq (coll (pair coll a))) => 
                  pair coll a -> pair coll a -> pair coll a

        aggSnd :: (Monoid a, Monoid (coll (Tree coll a)), Eq a) => 
                  pair coll a -> pair coll a -> pair coll a


-- | Group by the second component
groupSnd  :: (Agg pair, F.Foldable coll, Ord (pair coll a), Ord (coll (pair coll a)), Ord a) =>
                coll (pair coll a) -> [[pair coll a]]

groupSnd ts = groupBy eq $ sortBy cmpSnd $ F.toList ts
        where
            eq  x y = (cmpSnd x y) == EQ

groupFst ts = groupBy eq $ sortBy cmpFst $ F.toList ts
        where
            eq  x y = (cmpFst x y) == EQ

-- | Merge the first component
mergeFst xs = map (foldr aggFst (Leaf [])) xs

mergeSnd xs = map (foldr aggSnd (Leaf [])) xs


-- | Aggregate first component on equal seconds
aggregateFst xs = (mergeFst . groupSnd) xs


-- | Aggregate second component on equal firsts
aggregateSnd xs = (mergeSnd . groupFst) xs
 
-- | Example implementation: Tree
data Tree coll a = Node a (coll (Tree coll a)) | 
                   Leaf a

instance Agg Tree where
        cmpFst (Node x xs) (Node y ys) = compare x y
        cmpFst (Leaf x) (Node y ys)    = GT
        cmpFst (Node y ys) (Leaf x)    = LT
        cmpFst (Leaf x) (Leaf y)       = compare x y

   
        cmpSnd (Node x xs) (Node y ys) = compare xs ys 
        cmpSnd (Leaf x) (Node y ys)    = GT
        cmpSnd (Node y ys) (Leaf x)    = LT
        cmpSnd (Leaf x) (Leaf y)       = EQ

        aggFst (Node x xs) (Node y ys) 
                | xs == ys = Node (x<>y) xs
        aggFst (Leaf x) (Leaf y) = Leaf (x<>y) 
        aggFst (Leaf x) (Node _ _) = Leaf x
        aggFst (Node _ _) (Leaf y) = Leaf y

        aggSnd (Node x xs) y =
                case y of
                    Node y ys -> assert (x==y) (Node y (xs <> ys))
                    Leaf mempty   -> Node x xs
        aggSnd  (Leaf x) (Leaf y) = assert (x == y || y==mempty) (Leaf x)



instance Eq (Tree [] [String]) where
        (Node a as) == (Node b bs) = a==b && as==bs
        (Leaf a)    == (Leaf b)    = a==b 

instance Ord (Tree [] [String]) where
        compare (Node a as) (Node b bs) = compare a b <> compare as bs
        compare (Leaf a) (Leaf b) = compare a b 


instance Show (Tree [] [String])
        where
            show (Node a as) = "Node " ++ show a ++ show as
            show (Leaf a) = "Leaf " ++ show a


ex_tree1 = [
 Leaf ["foo"], 
 Leaf ["bar"] 
 ] :: [Tree [] [String]]

ex_tree2 = [
 Node ["foo"] [Leaf ["leaf1"]] , 
 Node ["foo"] [Leaf ["leaf1"]]
 ] :: [Tree [] [String]]

xxx = aggregateFst ex_tree1
xxy = aggregateSnd ex_tree2
