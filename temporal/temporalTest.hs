import Temporal
import Test.QuickCheck
import Data.List
import Control.Monad
import Control.Applicative
import Text.PrettyPrint.GenericPretty
import qualified Data.Foldable as F
------------------------------------------------------------
-- Temporal
------------------------------------------------------------

instance Arbitrary Time
        where
            arbitrary= do
                t <- arbitrary
                return (T t)


instance (Arbitrary a, Eq a, Ord a) => Arbitrary (Temporal a)
        where
            arbitrary = do
                ts <- orderedList
                (NonEmpty vs) <- arbitrary
                return $ Temporal (zip (DPast:(nub ts)) vs)

prop_foldable :: (Temporal Int) -> Bool
prop_foldable tpr = F.foldr (*) 0 tpr == 0

prop_applicative1 :: (Temporal Int) -> Bool
prop_applicative1 tpr1 = ((+) <$> tpr1 <*> tpr1) == (fmap (*2) tpr1)

prop_applicative2 :: (Temporal Int) -> Bool
prop_applicative2 tpr1 = ((*) <$> tpr1 <*> tpr1) == (fmap (^2) tpr1)


prop_applicative3 :: (Temporal Int) -> (Temporal Int) -> Bool
prop_applicative3 tpr1 tpr2 = ((*) <$> tpr1 <*> tpr2) == ((*) <$> tpr2 <*> tpr1)


prop_tJoin :: (Temporal Int) -> (Temporal Int) -> Bool
prop_tJoin tpr1 tpr2 = let f  = (*)
                           y1 = f <$> tpr1 <*>  tpr2
                           y2 = (f <$> tpr1) `ap` tpr2 
                       in y1 == y2

{-
instance (Arbitrary a, Eq a) => Arbitrary (Temporal a)
        where
            arbitrary = do
                d  <- arbitrary
                (CList cs) <- arbitrary 
                return (Temporal d cs)

-- sample (arbitrary :: Gen (Temporal Int))

prop_rightid :: (Temporal Int) -> Bool
prop_rightid tpr = (tpr >>= return) == tpr

prop_leftid :: (Temporal Int) -> Int -> Bool
prop_leftid tpr x = let f = (\x-> (fmap (*x) tpr))
                    in (return x >>= f) == f x

prop_tJoin :: (Temporal Int) -> (Temporal Int) -> Bool
prop_tJoin tpr1 tpr2 = let f  = (*)
                           y1 = f <$> tpr1 <*>  tpr2
                           y2 = (f <$> tpr1) `ap` tpr2 
                       in y1 == y2

-}
shortCheck prop = quickCheckWith stdArgs { maxSuccess = 20} prop



testTemporal = do
    putStrLn "\nTesting Temporal"
    shortCheck prop_foldable
    shortCheck prop_applicative1 
    shortCheck prop_applicative2 
    shortCheck prop_applicative3
    shortCheck prop_tJoin



main :: IO()
main = do
    testTemporal

