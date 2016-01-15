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

type Ranges a b = M.Map a (S.Set (b,b))
type AllCodes a = S.Set a

addRange :: (Ord a, Ord b) => a -> (b,b) ->  Ranges a b -> Reader (AllCodes b) (Ranges a b)
addRange a (l,h) rs = 
    case M.lookup a rs  of
        Nothing -> return $ M.insert a (S.singleton (l,h)) rs
        Just bs -> do
                      r' <- extend bs (l,h)
                      return $ M.insert a r' rs


extend :: (Ord b) => S.Set (b,b) -> (b,b) -> Reader (AllCodes b) (S.Set (b,b))
extend s (l,h) 
        | S.null lower = return upper
        | S.null upper = return lower
        | otherwise = return $ low ++ high 
                where
                    (lower, upper) = S.split (l,h) s
                    low            = S.deleteMax lower
                    (ll,lh)        = S.findMax lower
                    high           = S.deleteMin upper
                    (ul,uh)        = S.findMin upper
                    (++)           = S.union
                    low'           = do
                        g <- isGap lh l
                        if g then return (ll, h) else undefined

combine :: (Ord b) => S.Set (b,b) -> S.Set (b,b) -> Reader (AllCodes b) (S.Set (b,b))
combine xs ys
        | S.null xs = return ys
        | S.null ys = return xs
        | otherwise = return $ lowXs ++ combined ++ highYs
        where
            (++)   = S.union
            lowXs  = S.deleteMax xs
            highYs = S.deleteMin ys
            xmax   = S.findMax xs
            ymin   = S.findMin ys 
            combined = undefined

isGap :: (Ord a) => a -> a -> Reader (AllCodes a) Bool
isGap l h = do
    allBs <- ask
    case S.lookupGT l allBs of
        Nothing -> return True
        Just x  -> return (x >= h)

{-
addRange :: (Ord a, Ord b) => AllCodes b -> a -> (b,b) -> Ranges a b -> Ranges a b
addRange allBs a (l,h) rs = case M.lookup a rs  of
                                Nothing -> M.insert a (S.singleton (l,h)) rs
                                Just bs -> let (++) = extend allBs
                                               (below,_)  = S.split (l,l) bs
                                               (_, above) = S.split (h,h) bs
                                           in M.insert a (below ++ S.singleton (l,h)) rs


extend :: (Ord b) => S.Set b -> S.Set (b,b) -> S.Set (b,b) -> S.Set (b,b)
extend allBs sl sh
        | S.null sl = sh
        | S.null sh = sl
        | otherwise = let (++) = S.union
                          (ll, lh) = S.findMax sl
                          (hl, hh) = S.findMin sh
                          xtended  = S.singleton (ll, hh)
                      in if isGap allBs lh hl
                         then (S.deleteMax sl) ++ xtended ++ (S.deleteMin sh)
                         else sl ++ sh
-}


ex_b = S.fromList $ join [[n..n+10] | n <- [0,20..100]]
ex_r = M.fromList [
        ("foo",
              S.fromList[
                        (1,10),
                        (20,30)
                       ]
        ),
        ("bar",
              S.fromList[
                        (1,10),
                        (20,30)
                       ]
        )
       ]

xxx = runReader (addRange "foo" (21,24) ex_r) ex_b

x1 = S.fromList [(1,10),(20,30)]
