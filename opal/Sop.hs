import qualified Data.List as L
import qualified Data.Set as S
import qualified Data.Map as M
import Text.Show.Pretty


import Data.Maybe
import Data.Function
import Control.Monad
import Control.Monad.Reader
import Debug.Trace

pp x = putStrLn $ show x
ts x = trace ("*** " ++ show x ++ " ***") x

type Range b = (b,b)
type RangeSet b = S.Set (Range b)
type RangeList b = [Range b]
type RangeMap a b = M.Map a (RangeSet b)
type AllCodes a = S.Set a

-- | Add a Range to a RangeMap
addRange :: (Ord a, Ord b, Show a, Show b) => a -> (b,b) ->  RangeMap a b -> Reader (AllCodes b) (RangeMap a b)
addRange a (l,h) rs = 
    case M.lookup a rs  of
      -- xxx better just do the map lookup and delegate everything else
        Nothing -> return $ M.insert a (S.singleton (l,h)) rs
        Just bs -> do
          let (lo,mid,hi) = splitRanges bs (l,h)
          mid' <- combine mid
          return $ M.insert a (S.unions [lo, S.fromAscList mid', hi]) rs

-- | Combine Ranges in a RangeList to a new (smaller) RangeList
combine :: (Ord b, Show b) => (RangeList b) -> Reader (AllCodes b) (RangeList b)
combine [] = return []
combine (b:bs) = do
                lresult <- foldM mrg (b,[]) bs
                return $ addLast lresult
  where
    addLast (x,xs) = (x:xs)
    mrg :: (Ord a) => (Range a, RangeList a) -> Range a -> Reader (AllCodes a) (Range a, RangeList a)
    mrg ((lo,hi),xs) (lo', hi') = do
      allBs <- ask
      gap   <- isGap hi lo'
      if gap
        then return ((lo, max hi hi'), xs)   -- extend range
        else return ((lo', hi'), (lo,hi):xs) -- close range


-- | Take a new Range and split a RangeSet into three parts, such that
-- only the middle part has a change of being affected by the new
-- range
splitRanges :: (Ord b, Show b) => RangeSet b -> (b, b) -> (RangeSet b, [(b,b)] , RangeSet b)
splitRanges rs (lo,hi) = let 
                           toEmpty (Just (x, s)) = (S.singleton x, s)
                           toEmpty Nothing = (S.empty, S.empty)
                           (lower, rest) = S.split (lo,lo) rs
                           (mid, upper)  = S.split (hi,hi) rest -- xxx wrong
                           (maxLow, lower') = toEmpty (S.maxView lower)
                           (minUpr, upper') = toEmpty (S.minView upper)
                           -- mid' are the ranges which may get merged, upper' and lower' are certainly unaffected
                           mid' = S.toAscList (S.unions [maxLow, mid, minUpr,  S.singleton (lo,hi)])
                       in (lower', mid', upper')


isGap :: (Ord a) => a -> a -> Reader (AllCodes a) Bool
isGap l h = do
    allBs <- ask
    case S.lookupGT l allBs of
        Nothing -> return True
        Just x  -> return (x >= h)



ex_b = S.fromList $ join [[n..n+10] | n <- [0,20..100]]
ex_r = M.fromList [
        ("foo",
              S.fromList[
                        (1,10),
                        (25,30)
                       ]
        ),
        ("bar",
              S.fromList[(20*x, 20*x+10) | x<-[1..10000]]
        )
       ]

xxx r = runReader (addRange "bar" r ex_r) ex_b

x1 = S.fromList [(1,10),(20,30)]
