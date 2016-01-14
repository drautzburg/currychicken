import qualified Data.List as L
import Data.Function

class Predicate p where
  sat :: (Eq a, Ord a) => p a -> a -> Bool

class Combinable c where
  cUnion :: (Eq a, Ord a) => [c a] -> [c a]

data Range a b = Range {
        a :: a,
        r :: (b,b)
        }
               deriving (Eq, Show)
                        
instance (Eq a) => Combinable (Range a)
  where
    cUnion xs =  map f gs
      where
        gs = L.groupBy ((==) `on` a) xs
        -- [[Range {a = 1, r = (10,20)},Range {a = 1, r = (15,25)}]
        f ys = foldr1 g ys 
        g (Range a1 (l1,h1)) (Range a2 (l2,h2)) = Range a1 ((min l1 l2),(max h1 h2))


instance Predicate (Range a) where
  sat (Range a (l,h)) x = x >= l && x <= h

xxx = [Range 1 (10,20), Range 1 (15,25), Range 2 (30,40)]
