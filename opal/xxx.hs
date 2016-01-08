import Data.List
import Data.Maybe
import Text.Show.Pretty

pp x = putStrLn $ ppShow x

data Item lty = Ipacked lty [Item lty] | 
                Inonempty lty
              deriving (Eq, Ord, Show)

class Product prod where
        sat :: (Ord lty) => (Item lty) -> prod lty -> Bool


data Plist a    = Plist [Packed a] | PlAny | PlNone
                deriving (Eq, Ord, Show)
data Packed a   = Packed a (Plist a)
                deriving (Eq, Ord, Show)
data ListProd a = Pl (Plist a) | Pd (Packed a)
                deriving (Eq, Ord, Show)


-- | Build the union of two Plists
plsUnion :: Ord a => Plist a -> Plist a -> Plist a
plsUnion PlAny _ = PlAny
plsUnion _ PlAny = PlAny
plsUnion PlNone x = x
plsUnion x PlNone = x
plsUnion (Plist as) (Plist bs) = Plist (undupe$ as ++ bs)
  where
    undupe = nub . sort

-- | Intersect two Packed
pckIntersect :: (Ord a) => Packed a -> Packed a -> Maybe (Packed a)
pckIntersect (Packed a as) (Packed b bs)
  | a == b    = Just $ Packed a (plsIntersect as bs)
  | otherwise = Nothing

-- | Filter a Plist by a Packed
plsFilter :: (Ord a) => Packed a -> Plist a -> Plist a
plsFilter pck PlAny = Plist [pck]
plsFilter _ PlNone  = PlNone
plsFilter pck (Plist pcks) = Plist $ foldr f [] pcks
  where
    f pck' ys = case pckIntersect pck pck' of
      (Just y') -> y':ys
      otherwise -> ys

-- | Interesct two Plists
plsIntersect :: Ord a => Plist a -> Plist a -> Plist a
plsIntersect PlAny x = x
plsIntersect PlNone x = PlNone
plsIntersect x PlAny = x
plsIntersect x PlNone = PlNone
plsIntersect (Plist pcks1) pls = foldr plsUnion (Plist []) $ do
  pck1 <- pcks1
  return $ plsFilter pck1 pls

-- | compute the input product of a split
split :: (Ord a) => [Plist a] -> Plist a
split pxs = foldr plsUnion (Plist []) pxs

-- | compute the input products of a merge
merge :: (Eq a, Ord a) => [Packed a] -> Plist a -> [Plist a]
merge pcks pls = map f pcks
  where
    f p = plsFilter p pls

-- | compute container label and input Plist of a Pack
pack :: Packed a -> (a, Plist a)
pack (Packed a as) = (a, as)

-- compute input Product of an unpack
unpack :: a -> Plist a -> Packed a
unpack a pls = Packed a pls


ex_list1 = Plist [Packed ("CTY"++ (show x)) PlAny | x<-[1..10]]
ex_list2 = Plist [Packed ("CTY"++ (show x)) PlAny | x<-[5..15]]
ex_packed = Packed "CTY1" PlAny
