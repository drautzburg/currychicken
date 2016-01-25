import Control.Applicative
import qualified Data.Foldable as F

data Path a = Open | Closed | Path a (Path a)

fromListOpen [] = Open
fromListOpen (x:xs) = Path x (fromListOpen xs)

fromListClosed [] = Closed
fromListClosed (x:xs) = Path x (fromListClosed xs)

instance (Show a) => Show (Path a) where
  show p = "[" ++ sh p ++ "]"
    where
      sh Open = "..."
      sh Closed = "#"
      sh (Path x xs) = show x ++ ("," ++ (sh xs ))

instance Functor Path where
  fmap f Open   = Open
  fmap f Closed = Closed
  fmap f (Path x xs) = Path (f x) (fmap f xs)

instance F.Foldable Path where
  foldr f a Open = a
  foldr f a Closed = a
  foldr f a (Path x xs) = F.foldr f (f x a) xs 

-- alternatively

data Path1 a = Open' [a] | Closed' [a]
 
