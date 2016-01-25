import Control.Applicative
import qualified Data.Foldable as F
import qualified Data.Set as S
import qualified Data.List as L
import qualified Data.Monoid as M
import Data.Maybe
import Control.Monad

data Path1 a = Open1 | Closed1 | Path1 a (Path1 a)

fromListOpen1 [] = Open1
fromListOpen1 (x:xs) = Path1 x (fromListOpen1 xs)

fromListClosed1 [] = Closed1
fromListClosed1 (x:xs) = Path1 x (fromListClosed1 xs)

instance (Show a) => Show (Path1 a) where
  show p = "[" ++ sh p ++ "]"
    where
      sh Open1 = "..."
      sh Closed1 = "#"
      sh (Path1 x xs) = show x ++ ("," ++ (sh xs ))

instance Functor Path1 where
  fmap f Open1   = Open1
  fmap f Closed1 = Closed1
  fmap f (Path1 x xs) = Path1 (f x) (fmap f xs)

instance F.Foldable Path1 where
  foldr f a Open1 = a
  foldr f a Closed1 = a
  foldr f a (Path1 x xs) = F.foldr f (f x a) xs 

------------------------------------------------------------
-- alternatively
------------------------------------------------------------

data Path a = Open [a] | Closed [a]
            deriving (Eq, Show)
pathList (Open as)   = as
pathList (Closed as) = as 



instance (Ord a) => Ord (Path a) where
        compare (Open a) (Open b) = compare a b
        compare (Closed a) (Closed b) = compare a b
        compare (Open a) (Closed b)  = compare a b `M.mappend` LT
        compare (Closed a) (Open b)  = compare a b `M.mappend` GT
        

-- first path includes second?
pIncludes :: (Eq a) => 
             Path a -> Path a -> Bool
pIncludes (Closed a) (Open b) = False -- xxx finish
pIncludes (Open a) (Open b) = L.isPrefixOf a b


pAccepts :: (Eq a) => (Path a) -> (Path a) -> Bool
pAccepts (Open i) (Closed p) = False
pAccepts item prod           = pathList prod `L.isPrefixOf` pathList item


------------------------------------------------------------
-- Processes
------------------------------------------------------------

split ::  (M.Monoid (coll (Path a)))=> 
          [coll (Path a)] -> coll (Path a)
split ps = foldr (M.mappend) M.mempty ps

restrict :: (Eq a, Ord a) 
            => a -> S.Set (Path a) -> S.Set (Path a) 
restrict a ps = S.foldr f S.empty ps
        where
            f path set = 
                    case path of
                        Open   []    -> S.insert (Open [a]) set
                        Closed []    -> set
                        Open(x:xs)   -> if (x==a) then S.insert (Open(x:xs)) set   else set
                        Closed(x:xs) -> if (x==a) then S.insert (Closed(x:xs)) set else set






ex_paths = S.fromList (Open[1] 
                                   : 
                                   [f xs |
                                    f <- [Open, Closed],
                                    xs <- [[a,200,300] | a <- [1..2]]
                                   ]
                      )








