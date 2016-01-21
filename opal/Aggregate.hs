import Data.Monoid
import Data.List
import Data.Ord
import Data.Tuple
import qualified Data.Set as S
import Data.Function

aggregate :: (Ord a, Monoid mb) => [(a,mb)] -> [(a, mb)]
aggregate = map merge_b . group_a 
  where
    group_a = groupBy ((==) `on` fst) . sortBy (comparing fst)
    merge_b (x:xs) = foldr add x xs
    add (xa, xb) (ya, yb)
      | xa == xa = (xa, xb <> yb)

xxx = map swap $ aggregate $ map swap $ aggregate [
  (S.fromList [1], S.fromList [10]),
  (S.fromList [1], S.fromList [20]),
  (S.fromList [2], S.fromList [10,21])]


