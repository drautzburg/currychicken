import qualified Data.List as L
import qualified Data.Set as S
import qualified Data.Map as M
import Test.QuickCheck
import Data.Maybe
import Text.Show.Pretty

pp a = putStrLn $ ppShow a



-- | An Item is a container with a label and other items
-- inside. Should we not care about what's inside, we use Inonempty to
-- distinguish it from an empty container (where we known what's
-- inside, namely nothing)
data Item lbl = Ipacked lbl [Item lbl] | 
                Inonempty lbl
                          deriving (Eq, Ord, Show)

-- | An empty container
iempty lbl   = Ipacked lbl []

-- | A Product tells us whether or not an Item statisfies it. There
-- can be various implementations, hence this is a typeclass. The
-- labels must allow ordering.
class Product p where
        sat :: (Ord a) => p a -> (Item a) -> Bool

-- | A possible implementation of Product is a Map, which maps item
-- labels to another MapProduct, which determines the possible
-- contained Items. A second constructor MPany expresses that we
-- accept any Item.

data  MapProduct a = MPacked  (M.Map a (MapProduct a)) | 
                     MPany deriving (Show)

-- | A MapProduct matching 'Inonempty'
mpNonempty a = MPacked (M.singleton a MPany)

-- | This allows us to implement 'sat'
instance Product MapProduct
        where
            sat MPany _ = True
            sat (MPacked map) (Ipacked lbl is) 
                    = case M.lookup lbl map of
                          Nothing -> False
                          Just mp -> all (sat mp) is
            sat (MPacked map) (Inonempty lbl)
                    = case M.lookup lbl map of
                          Just MPany -> True
                          _          -> False

showTree (MPacked map) = putStrLn $ M.showTree map




mpSplit :: (Ord a) => [MapProduct a] -> MapProduct a
mpSplit mps = foldr u (MPacked M.empty) mps
        where
            u MPany _ = MPany
            u _ MPany = MPany
            u (MPacked xm) (MPacked ym) = MPacked (M.union xm ym)


mpUnpack :: (Ord a) => a -> [MapProduct a] -> MapProduct a
mpUnpack a ps = MPacked $ M.singleton a (mpSplit ps)

mpPack :: (Ord a) => MapProduct a -> ([a], MapProduct a)
mpPack (MPacked map) = (M.keys map, mpSplit $ M.elems map)

mp = mpSplit [
      mpNonempty "foo", 
      mpUnpack "bar" [
       mpNonempty "bar1", 
       mpNonempty "bar2"
      ]
     ]


-- iPack :: a -> [Item a] -> Item a
-- ipack a is = Ipacked a 

it = Inonempty "bar"
ie = iempty "bar"
ix = Ipacked "bar" [iempty "bar1"]


newtype Fan = Fan Int deriving (Eq, Ord, Show)

instance Arbitrary Fan where
        arbitrary = do
            x <- arbitrary :: (Gen Int)
            return (Fan (abs x + 1))
