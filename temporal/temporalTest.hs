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
-- sample (arbitrary :: Gen Time)

instance Arbitrary Time
        where
            arbitrary= do
                t <- arbitrary
                frequency [(1, return DPast), (10, return $ T t)]


instance (Arbitrary a, Eq a, Ord a) => Arbitrary (Temporal a)
        where
            arbitrary = do
                ts <- orderedList
                (NonEmpty vs) <- arbitrary
                return $ Temporal (zip (nub (DPast:ts)) vs)

prop_foldable :: (Temporal Int) -> Bool
prop_foldable tpr = F.foldr (*) 0 tpr == 0

prop_applicative1 :: (Temporal Int) -> Bool
prop_applicative1 tpr1 = ((+) <$> tpr1 <*> tpr1) == (fmap (*2) tpr1)

prop_applicative2 :: (Temporal Int) -> Bool
prop_applicative2 tpr1 = ((*) <$> tpr1 <*> tpr1) == (fmap (^2) tpr1)


prop_applicative3 :: (Temporal Int) -> (Temporal Int) -> Bool
prop_applicative3 tpr1 tpr2 = ((*) <$> tpr1 <*> tpr2) == ((*) <$> tpr2 <*> tpr1)

prop_rightid :: (Temporal Int) -> Bool
prop_rightid tpr = (tpr >>= return) == tpr

prop_leftid :: (Temporal Int) -> Int -> Bool
prop_leftid tpr x = let f = (\x-> (fmap (*x) tpr))
                    in (return x >>= f) == f x

prop_bind :: (Temporal Int) -> (Temporal Int) -> Bool
prop_bind tpr1 tpr2 = let f  = (*)
                          y1 = f <$> tpr1 <*>  tpr2
                          y2 = (f <$> tpr1) `ap` tpr2 
                      in y1 == y2


shortCheck name prop = do
  putStr $ name ++ "\t"
  quickCheckWith stdArgs { maxSuccess = 20} prop


main :: IO()
main = do
    putStrLn "\nTesting Temporal"
    shortCheck "fold" prop_foldable
    shortCheck "app1"prop_applicative1 
    shortCheck "app2" prop_applicative2 
    shortCheck "app3" prop_applicative3
    shortCheck "rtid" prop_rightid
    shortCheck "ltid" prop_leftid
    shortCheck "bind" prop_bind

