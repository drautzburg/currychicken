{-# Language FlexibleInstances #-}
import qualified Data.List as L
import Control.Monad
import Data.Maybe

data Tree a = Tree a [Tree a] | Node a 
               deriving (Eq, Ord, Show)

type Item a = Tree a

------------------------------------------------------------
class Checkable a where
  cSat :: (Ord a) => [a] -> a -> Bool
  cAnd :: (Ord a) => [a] -> [a]
  cOr :: (Ord a) => [a] -> [a]
  cFilter :: (Ord a) => [a] -> (a->Bool) -> [a]



class Predicate p where
  prSat     :: (Eq a) => p a -> a -> Bool
  prAnd     :: (Eq a) => p a -> p a -> Maybe (p a)
  prShow    :: (Show a) => p a -> String


data LblList lty = LblList [lty]
                   deriving (Show)

instance Predicate LblList where
  prSat  (LblList lbls) lbl = lbl `L.elem` lbls
  prAnd  (LblList lbls1) (LblList lbls2) =
    case L.intersect lbls1 lbls2 of
      [] -> Nothing
      ys -> Just (LblList ys)
  prShow = show

------------------------------------------------------------  

matches :: (Eq a) => (Predicate p) => Item a -> Tree (p a) -> Bool
matches (Node i) (Node p) = prSat p i
matches (Tree i is) (Node p) = prSat p i
matches (Tree i is) (Tree p ps) = prSat p i &&
                                  all (flip matchesAny $ ps ) is

matchesAny :: (Eq a, Predicate p) => Item a -> [Tree (p a)] -> Bool
matchesAny i ps = any (matches i) ps

------------------------------------------------------------

split :: (Predicate p, Eq a) => [[Tree (p a)]] -> [Tree (p a)]
split as = join as

restrict :: (Predicate p, Eq a) => p a -> Tree (p a) -> Maybe (Tree (p a))
restrict p1 (Node p2) = case prAnd p1 p2 of
  Nothing -> Nothing
  Just py -> Just (Node py)
restrict p1 (Tree p2 _) = case prAnd p1 p2 of
  Nothing -> Nothing
  Just py -> Just (Node py)


restrictAll :: (Predicate p, Eq a) => p a -> [Tree (p a)] -> [Tree (p a)]
restrictAll p ps = catMaybes $ map (restrict p) ps

merge :: (Predicate p, Eq a) => [p a] -> [Tree (p a)] -> [[Tree (p a)]]
merge ps ts = map (flip restrictAll $ ts) ps


