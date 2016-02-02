{-# Language FlexibleInstances #-}
{-# Language UndecidableInstances #-}

import qualified Data.List as L
import qualified Data.Set as S
import Debug.Trace
import Test.QuickCheck hiding ((==>))

------------------------------------------------------------
data Nest lty = Nest [lty] deriving (Eq, Ord, Show)
------------------------------------------------------------

-- can implement element for Nest
nElement :: (Eq lty) => Nest lty -> Nest lty -> Bool
nElement = (==)

------------------------------------------------------------
data Cart lty = Cart [[lty]] deriving (Eq, Ord, Show)
------------------------------------------------------------

-- can implement element for Cart
cElement :: (Eq lty) => Nest lty -> Cart lty -> Bool
cElement (Nest []) (Cart cs) = cs == [] -- Closed
cElement (Nest (l:ls)) (Cart (c:cs)) = l `elem` c && (Nest ls) `cElement` (Cart cs)
cElement _ _ = False

-- can put a single item into a Cart
singleCart :: Nest lty -> Cart lty
singleCart (Nest lbls) = Cart (map return lbls)

------------------------------------------------------------
data Nitems lty = Nitems (S.Set (Nest lty)) deriving Show
------------------------------------------------------------
-- can put a single item into Nitems
singleNitems :: Nest lty -> Nitems lty
singleNitems (Nest lbls) = Nitems $ S.singleton $ Nest lbls

------------------------------------------------------------
data Citems lty = Citems (S.Set (Cart lty)) deriving Show
------------------------------------------------------------
-- can put a single item into Citems
singleCitems :: Nest lty -> Citems lty
singleCitems (Nest lbls) = Citems $ S.singleton $ singleCart $ Nest lbls


------------------------------------------------------------
class Container cnt where
------------------------------------------------------------
  single :: Nest lty -> cnt lty
  elemt  :: (Eq lty, Ord lty) => Nest lty -> cnt lty -> Bool

instance Container Nest   where
  single = id
  elemt  = (==)
  
instance Container Cart   where
  single = singleCart
  elemt  = cElement

instance Container Nitems where
  single = singleNitems
  elemt  x (Nitems xs) = S.member x xs

instance Container Citems where
  single = singleCitems
  elemt x (Citems xs) = S.foldr f False xs
    where
      f cs False = x `cElement` cs
      f _  True  = True
