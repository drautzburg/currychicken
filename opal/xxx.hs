import Text.Show.Pretty 
import Test.QuickCheck hiding ((==>))
import Control.Monad
import Data.List hiding (union, intersection)
import qualified Data.Map as M 

pp a = putStrLn $ ppShow a



data Product lty = Anything | Choice (M.Map lty (Product lty)) deriving (Eq, Show)

fromList ls = Choice $ M.fromList ls

nothing :: (Ord lty) => Product lty
nothing = Choice $ M.fromList []

union Anything _ = Anything
union _ Anything = Anything
union (Choice p1) (Choice p2) = 
        Choice $ M.unionWith union p1 p2

intersection Anything x = x
intersection x Anything = x
intersection (Choice p1) (Choice p2) = 
        Choice $ M.intersectionWith intersection p1 p2


------------------------------------------------------------
-- Tests
------------------------------------------------------------

instance (Ord lty, Arbitrary lty) => Arbitrary (Product lty) where
        arbitrary = sized arbProd
                    where
                        arbProd :: (Ord lty, Arbitrary lty) => Int -> Gen (Product lty)
                        arbProd 0 = elements [Anything, fromList []]
                        arbProd n = do
                            (Positive m) <- arbitrary
                            let n' = n `div` (m+1)
                            cont <- replicateM m (arbProd n')
                            lbls <- arbitrary
                            return $ fromList $ zip lbls cont
                            

xxx = do
    ps <- sample' (arbitrary :: Gen (Product Int))
    mapM_ pp ps


a ==> b = not a || b 
infixr 1 ==>

prop_union_comm :: Product Int -> Product Int -> Bool
prop_union_comm c1 c2 = union c1 c2 == union c2 c1

prop_union_assoc :: Product Int -> Product Int -> Product Int -> Bool
prop_union_assoc c1 c2 c3 = (c1 `union` c2) `union` c3 == c1 `union` (c2 `union` c3)

prop_intersection_comm :: Product Int -> Product Int -> Bool
prop_intersection_comm c1 c2 = intersection c1 c2 == intersection c2 c1

prop_intersection_assoc :: Product Int -> Product Int -> Product Int -> Bool
prop_intersection_assoc c1 c2 c3 = (c1 `intersection` c2) `intersection` c3 == c1 `intersection` (c2 `intersection` c3)

prop_distrib1 :: Product Int -> Product Int -> Product Int -> Bool
prop_distrib1 c1 c2 c3 = c1 `union` (c2 `intersection` c3) ==
                          (c1 `union` c2) `intersection` (c1 `union` c3)

prop_distrib2 :: Product Int -> Product Int -> Product Int -> Bool
prop_distrib2 c1 c2 c3 = c1 `intersection` (c2 `union` c3) ==
                          (c1 `intersection` c2) `union` (c1 `intersection` c3)

testAll = do
    runTest "union is commutative"           prop_union_comm
    runTest "union is associative"           prop_union_assoc
    runTest "isect is commutative"           prop_intersection_comm
    runTest "isect is associative"           prop_intersection_assoc
    runTest "union/isect is distributive1"   prop_distrib1
    runTest "union/isect is distributive2"   prop_distrib2
    where
       runTest name test = putStrLn "" >> putStrLn name >> quickCheck test

