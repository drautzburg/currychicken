
import qualified Data.List as L
import qualified Data.Set as S
import Test.QuickCheck
import Data.Maybe
import Text.Show.Pretty
import Control.Arrow
-- import Data.String.Utils
pp a = putStrLn $ ppShow a
lpp a = do
    putStrLn "\\begin{verbatim}"
    putStrLn $ ppShow a
    putStrLn "\\end{verbatim}"


data Item lty = Inest lty [Item lty] | 
                Inonempty lty
              deriving (Eq, Ord, Show)


class Product prod where
        accepts :: (Eq lty, Ord lty, Predicate p) => 
                   prod (p lty) -> Item lty -> Bool

newtype Foo p lty = Foo {acc :: lty -> Maybe (Foo p lty)}
xxx :: (Eq lty, Predicate p) => (Foo (p lty)) -> Item lty -> Bool
xxx (Foo acc) (Inonempty lbl) = case acc lbl of
                                    Just p -> prAny p


xxx (Foo acc) (Inest lbl is) = case acc lbl of
                                   Just p -> all (xxx p) is


------------------------------------------------------------
data Tree a = Tree a [Tree a] | Node a
               deriving (Eq, Ord, Show)
------------------------------------------------------------
-- xxx there is no choice, why a class?
-- xxx no need to make Item a newtype
data ProductRep p lty = ListRep [Tree (p lty)] | 
                        NestRep (Tree (p lty))
                                deriving (Show)

class Predicate ls where
  prSat     :: (Eq lty) => ls lty -> lty -> Bool
  prAny     :: ls lty -> Bool
  prShow    :: (Show lty) => ls lty -> String

-- predicate implemented as a list of labels
data Lpred lty = Lpred  [lty] | LpAny

instance Predicate Lpred where
  prSat   (Lpred as) a = a `L.elem` as
  prShow  (Lpred as) = show as
  prAny LpAny = True
  prAny _     = False

ex_foo =  Tree "foo" 
          [
            Node "foo1",
            Node "foo2" 
           ]


ex_bar =  Tree "bar" []

ex_plist1 = [ex_foo, ex_bar]


-- The whole Product is a |ListRep|.
ex_prod1 = ListRep ex_plist1


lAccepts :: (Ord lty, Predicate p) => [Tree (p lty)] -> Item lty -> Bool
lAccepts ps item = any (flip nAccepts $ item) ps

nAccepts :: (Ord lty, Predicate p) => Tree (p lty) -> Item lty -> Bool
nAccepts (Node p) (Inonempty ilbl)    = prSat p ilbl 
nAccepts (Tree p ps) (Inest ilbl items) = prSat p ilbl &&
                                          all (lAccepts ps) items
nAccepts _ _ = False


-- instance Product (ProductRep p) where
--        accepts (NestRep p) item = nAccepts p item
--        accepts (ListRep prod) item = lAccepts prod item
        

{-

lUnion :: Ord lty => [Pitem lty] -> [Pitem lty] -> [Pitem lty]
lUnion = L.union 

lFilter :: (Ord lty) => Pitem lty -> [Pitem lty] -> [Pitem lty]
lFilter pn pns = foldr f [] pns
  where
    f pn' ys = case nIntersection pn pn' of
      (Just y') -> y':ys
      otherwise -> ys


lIntersection :: Ord lty => [Pitem lty] -> [Pitem lty] -> [Pitem lty]
lIntersection pis pls = foldr lUnion [] $ do
  pck1 <- pis
  return $ lFilter pck1 pls


nIntersection :: (Ord lty) => Pitem lty -> Pitem lty -> Maybe (Pitem lty)
nIntersection (Pitem a as) (Pitem b bs)
  | a == b    = Just $ Pitem a (lIntersection as bs) 
  | otherwise = Nothing

-}
{-
split :: (Ord a)=> [Plist a] -> Plist a
split pxs = foldr lUnion (Plist []) pxs

merge :: (Eq a, Ord a) => [Plist a] -> Plist a -> [Plist a]
merge plss pls = map f plss
  where
    f p = lIntersection p pls

-- xxx
mergeAll :: (Eq a, Ord a) => Plist a ->[Pnest a]
mergeAll (Plist pns) = pns


pack :: Pnest lty -> (lty, Plist lty)
pack (Pnest lbl pls) = (lbl, pls)

unpack :: lty -> Plist lty -> Pnest lty
unpack lbl pls = Pnest lbl pls

leaf :: lty -> Plist lty
leaf lbl = Plist[Pnest lbl PlAny]

splitAll :: Ord a => [Pnest a] -> Plist a
splitAll ns = split $ map (\np -> Plist [np]) ns

type Ex_lbl = (String, String, String)
ex_truck = 
        let 
           --shorthands
           lbl s x y = s ++ (show x) ++ (show y)
           clbl s1 s2 n = (s1, s2 ++ (show n), "")
           range size n = [size*n .. size*(n+1)-1]
           ------------------------------------------------------------
           -- Receiver side
           ------------------------------------------------------------
           -- The nth route consists of the following addresses
           -- xxx split ord and prio
           rRoute route = split [leaf("Letter", lbl "Addr" route i, mclass)
                                         | i <-[1..10],
                                               mclass <- ["Ord", "Prio"]
                               ] :: Plist Ex_lbl
           -- Each route has its own |Pnest| tray product
           rTray route   = unpack (clbl "Tray" "Route" route) (rRoute route)      
                         :: Pnest Ex_lbl

           -- Each delivery office is responsible for 10 routes ...
           rDof dof    = splitAll [rTray route | route <- range 10 dof] 
                       :: Plist Ex_lbl

           -- ... and has a dedicated rollcontainer
           rRc n     = unpack (clbl "RollContainer" "DO" n) (rDof n)  
                     :: Pnest Ex_lbl

           -- a Region services 5 Delivery Offices ...
           rRegion reg = splitAll [rRc i | i <- range 5 reg]     
                       :: Plist Ex_lbl

           -- ... and has a truck bringing the rollcontainers
           rTruck n  = unpack (clbl "Truck" "Region" n) (rRegion n)   :: Pnest Ex_lbl

           ------------------------------------------------------------
           -- Now what happens at the departure side of the truck
           ------------------------------------------------------------
           -- The truck to region n gets packed
           sTruck n = pack(rTruck n)               :: (Ex_lbl, Plist Ex_lbl)

           -- Each rollcontainer comes from a different source ...
           sRcs reg  = mergeAll (snd (sTruck reg))     :: [(Pnest Ex_lbl)]

           -- ... where they get packed
           sDof reg dof  = pack (sRcs reg !! dof)          :: (Ex_lbl, Plist Ex_lbl)

           -- 

           in (sRcs 0) -- (stray 0) !! 0
-}
